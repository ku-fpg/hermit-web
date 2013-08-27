{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module HERMIT.Web where

import GhcPlugins hiding ((<>), liftIO, text, display)

import HERMIT.Dictionary
import HERMIT.External
import HERMIT.Interp
import HERMIT.Kure
import HERMIT.Parser
import HERMIT.Plugin
import HERMIT.PrettyPrinter.Common
import HERMIT.Shell.Command
import HERMIT.Shell.Externals
import HERMIT.Shell.Types
import HERMIT.Kernel
import HERMIT.Kernel.Scoped

import HERMIT.Web.JSON
import HERMIT.Web.Renderer
import HERMIT.Web.Types

import Blaze.ByteString.Builder (fromByteString)

import Control.Concurrent.MVar
import Control.Monad.Error
import Control.Monad.State.Lazy hiding (get, put)
import qualified Control.Monad.State.Lazy as State

import qualified Data.ByteString.Char8 as BS
import Data.Default
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text.Lazy as T

import Network.HTTP.Types (status200, status500)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Web.Scotty.Trans

-- Global state is threaded with an MVar
-- importantly, the web app code below can pretend
-- there is a normal state monad.
mkScottyApp :: WebAppState -> CommandLineState -> ScottyH () -> IO Wai.Application
mkScottyApp wst cst defs = do
    sync <- liftIO newEmptyMVar
    let runWebM :: WebM a -> IO a
        runWebM m = do
            (r,s) <- runCLMToIO cst $ flip runStateT wst $ runErrorT $ runWebT m
            case r of
                Left err -> error err
                Right (r,w) -> do liftIO $ putMVar sync (s,w)
                                  either (\_ -> fail "don't abort/resume outside of an action") return r
        runToIO :: WebM Wai.Response -> IO Wai.Response
        runToIO m = do
            (s,w) <- liftIO $ takeMVar sync
            (r,s') <- runCLMToIO s $ flip runStateT w $ runErrorT $ runWebT m
            case r of
                Left err -> fail err
                Right (r,w') -> do liftIO $ putMVar sync (s',w')
                                   either (handleError (cl_kernel s')) return r
    scottyAppT runWebM runToIO defs

handleError :: ScopedKernel -> WebAppError -> IO Wai.Response
handleError k WAEAbort = do
    abortS k 
    return $ Wai.ResponseBuilder status200 [("Content-Type","text/html")]
           $ fromByteString "HERMIT Aborting"
handleError k (WAEResume sast) = do
    resumeS k sast >>= runKureM return fail 
    return $ Wai.ResponseBuilder status200 [("Content-Type","text/html")]
           $ fromByteString "HERMIT Resuming"
handleError _ (WAEError str) = return $ Wai.ResponseBuilder status500 [("Content-Type","text/html")]
                                      $ fromByteString $ BS.pack str 

runCLMToIO :: CommandLineState -> CLM m a -> m (Either String a, CommandLineState)
runCLMToIO s = flip runStateT s . runErrorT . runCLM

plugin :: Plugin
plugin = hermitPlugin $ \ phaseInfo -> if phaseNum phaseInfo == 0 
                                       then scopedKernel . server
                                       else const return

server :: [CommandLineOption] -> ScopedKernel -> SAST -> IO ()
server _opts skernel sast = do
    let dict = mkDict $ shell_externals ++ externals

    let shellState = CommandLineState
                       { cl_cursor         = sast
                       , cl_pretty         = "clean"
                       , cl_pretty_opts    = def
                       , cl_render         = unicodeConsole
                       , cl_height         = 30
                       , cl_nav            = False
                       , cl_running_script = False
                       , cl_tick          = undefined -- TODO
                       , cl_corelint      = False
                       , cl_failhard      = False
                       , cl_window        = mempty
                       , cl_dict          = dict
                       , cl_scripts       = []
                       , cl_kernel        = skernel
                       , cl_initSAST      = sast
                       , cl_version       = VersionStore
                                              { vs_graph = []
                                              , vs_tags  = []
                                              }
                       }

    app <- mkScottyApp def shellState $ do
        get "/webstate" $ do
            st <- webm State.get
            html $ mconcat ["<html><body>"
                           ,"Hackers Interface:</br>"
                           , showText st
                           -- abort
                           ,"<form method=POST action=\"/connect\">"
                           ,"<input type=submit name=Go/>"
                           ,"</form>"
                           ,"</body></html>"]

        -- Generate a new user id and initial token.
        post "/connect" $ do
            m <- webm $ gets users
            let k = nextKey m
            webm $ modify $ \st -> st { users = Map.insert k 0 m }
            json $ Token k 0

        -- Run a command, returning the result.
        post "/command" $ do
            Command t cmd <- jsonData
            checkUser t

            case parseScript cmd of
                Left  str    -> raise $ T.pack $ "Parse failure: " ++ str
                Right script -> evalStmts script

            ast <- clm $ getAST
            json $ CommandResponse t ast

        -- Get the list of commands
        get "/commands" $ do
            t <- jsonData
            checkUser t
            json $ CommandList t $ [ CommandInfo (externName e) 
                                                 (unlines $ externHelp e) 
                                                 (externTags e) 
                                   | e <- shell_externals ++ externals ]

    liftIO $ Warp.run 3000 app

{-# INLINE showText #-}
showText :: Show a => a -> T.Text
showText = T.pack . show

nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

checkUser :: Token -> ActionH ()
checkUser (Token u _) = do
    m <- webm $ gets users
    when (not $ Map.member u m) (raise $ "user id " <> showText u <> " not found!") 
    return ()

evalStmts :: Script -> ActionH ()
evalStmts = mapM_ evalExpr

evalExpr :: ExprH -> ActionH ()
evalExpr expr = do
    dict <- clm $ gets cl_dict
    runKureM (\case
                 -- special case these so the MVar doesn't hang
                 MetaCommand Resume  -> clm (gets cl_cursor) >>= webm . throwError . WAEResume
                 MetaCommand Abort   -> webm $ throwError WAEAbort
                 KernelEffect effect -> clm $ performKernelEffect effect expr
                 ShellEffect effect  -> clm $ performShellEffect effect
                 QueryFun query      -> clm $ performQuery query
                 MetaCommand meta    -> clm $ performMetaCommand meta
             )
             (raise . T.pack)
             (interpExprH dict interpShellCommand expr)

getAST :: MonadIO m => CLM m [Glyph]
getAST = do
    st <- State.get
    focusPath <- getFocusPath
    let skernel = cl_kernel st
        ppOpts = (cl_pretty_opts st) { po_focus = Just focusPath }
    iokm2clm' "Rendering error: "
              (\doc -> let Glyphs gs = renderCode ppOpts doc in return gs)
              (toASTS skernel (cl_cursor st) >>= liftKureM >>= \ ast ->
                queryK (kernelS skernel) ast (extractT $ pathT (cl_window st) $ liftPrettyH ppOpts $ pretty st) (cl_kernel_env st))


