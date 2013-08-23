{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase, RankNTypes,
             GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module HERMIT.Web where

import GhcPlugins hiding ((<>), liftIO, text, display)

import HERMIT.Dictionary
import HERMIT.Interp
import HERMIT.Kure
import HERMIT.Parser
import HERMIT.Optimize
import HERMIT.Plugin
import HERMIT.PrettyPrinter.Common
import HERMIT.Shell.Command hiding (commandLine)
import HERMIT.Shell.Externals
import HERMIT.Shell.Types
import HERMIT.Web.JSON
import HERMIT.Kernel
import HERMIT.Kernel.Scoped

import Control.Concurrent.MVar
import Control.Monad.Error
import Control.Monad.State.Lazy hiding (get, put)
import qualified Control.Monad.State.Lazy as State

import Data.Default
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text.Lazy as T

import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

import Web.Scotty

newtype WebAppState = WebAppState { users :: Map.Map Integer Integer }
    deriving (Show)

instance Default WebAppState where
    def = WebAppState { users = Map.empty }

newtype WebT m a = WebT { runWebT :: StateT WebAppState m a }
    deriving (Monad, MonadIO, MonadState WebAppState)

instance MonadTrans WebT where
    lift = WebT . lift

type WebM = WebT (CLM IO)

type ScottyH a = ScottyT WebM a
type ActionH a = ActionT WebM a

instance Show InterpState where
    show = show . isAST

-- Global state is threaded with an MVar
-- importantly, the web app code below can pretend
-- there is a normal state monad.
mkScottyApp :: WebAppState -> CommandLineState -> ScottyH () -> IO Wai.Application
mkScottyApp wst cst defs = do
    sync <- liftIO newEmptyMVar
    let runWebM :: WebM a -> IO a
        runWebM m = do
            (r,s) <- runCLMToIO cst $ runStateT (runWebT m) wst
            case r of
                Left err -> error err
                Right (r,w) -> do liftIO $ putMVar sync (s,w)
                                  return r
        runToIO :: WebM a -> IO a
        runToIO m = do
            (s,w) <- liftIO $ takeMVar sync
            (r,s') <- runCLMToIO s $ runStateT (runWebT m) w
            case r of
                Left err -> error err
                Right (r,w') -> do liftIO $ putMVar sync (s',w')
                                   return r
    scottyApp runWebM runToIO defs

runCLMToIO s = flip runStateT s . runErrorT . runCLM

-- The monad transformer stack is quite ridiculous at this point.
-- So here are some helpers to get things to the right place.

-- Do something in the CLM IO monad (the HERMIT plugin monad, see HERMIT.Optimize)
clm :: MonadTrans t => CLM IO a -> t WebM a
clm = lift . lift

-- Do something to the web application state.
webm :: MonadTrans t => WebM a -> t WebM a
webm = lift

plugin :: Plugin
plugin = hermitPlugin $ \ _pi -> scopedKernel . commandLine
--plugin = optimize server

--server :: [CommandLineOption] -> CLM IO ()
--server _opts = firstPhase $ do
commandLine :: [CommandLineOption] -> ScopedKernel -> SAST -> IO ()
commandLine _opts skernel sast = do
    let dict = mkDict shell_externals

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

        post "/connect" $ do
            m <- webm $ gets users
            let k = nextKey m
            webm $ modify $ \st -> st { users = Map.insert k 0 m }
            json $ Token k 0

        post "/command" $ do
            Command t cmd path <- jsonData
            t' <- checkToken t

            case parseScript cmd of
                Left  str    -> raise $ T.pack $ "Parse failure: " ++ str
                Right script -> evalStmts script

            ast <- clm $ getAST
            json $ CommandResponse t' ast path
{-

        get "/commands" $ do
            json $ mconcat []

        get "/complete" $ do
            query <- jsonData
            json $ mconcat []

        get "/reset" $ do
            t <- jsonData
            json $ t { tToken = 0 }
-}

    liftIO $ Warp.run 3000 app

{-# INLINE showText #-}
showText :: Show a => a -> T.Text
showText = T.pack . show

type UniqueId = Integer
type TokenNum = Integer

nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

checkToken :: Token -> ActionH Token
checkToken (Token u t) = do
    m <- webm $ gets users
    t' <- maybe (raise "user id not found!") return $ Map.lookup u m
    guardMsg (t >= t') "token out of order"
    webm $ modify $ \ st -> st { users = Map.adjust (+1) u m }
    return $ Token u (t'+1)

evalStmts :: Script -> ActionH ()
evalStmts = mapM_ evalExpr

evalExpr :: ExprH -> ActionH ()
evalExpr expr = do
    let dict = mkDict shell_externals
    runKureM (\case
                 KernelEffect effect -> clm $ performKernelEffect effect expr
                 ShellEffect effect  -> clm $ performShellEffect effect
                 QueryFun query      -> clm $ performQuery query
                 MetaCommand meta    -> clm $ performMetaCommand meta
             )
             (raise . T.pack)
             (interpExprH dict interpShellCommand expr)

getAST :: MonadIO m => CLM m String
getAST = do
    st <- State.get
    focusPath <- getFocusPath
    let skernel = cl_kernel st
        ppOpts = (cl_pretty_opts st) { po_focus = Just focusPath }
    iokm2clm' "Rendering error: "
              (\doc -> let HTML str = renderCode ppOpts doc in return str)
              (toASTS skernel (cl_cursor st) >>= liftKureM >>= \ ast ->
                queryK (kernelS skernel) ast (extractT $ pathT (cl_window st) $ liftPrettyH ppOpts $ pretty st) (cl_kernel_env st))


