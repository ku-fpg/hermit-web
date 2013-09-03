{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module HERMIT.Web (plugin) where

import           Blaze.ByteString.Builder (fromLazyByteString)

import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State

import qualified Data.Aeson as Aeson
import           Data.Default
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text.Lazy as T

import           GhcPlugins hiding ((<>), liftIO)

import           HERMIT.Dictionary
import           HERMIT.External
import           HERMIT.Interp
import           HERMIT.Kure
import           HERMIT.Parser
import           HERMIT.Plugin
import           HERMIT.PrettyPrinter.Common
import qualified HERMIT.PrettyPrinter.Clean as Clean
import           HERMIT.Shell.Command
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types
import           HERMIT.Kernel
import           HERMIT.Kernel.Scoped

import           HERMIT.Web.JSON
import           HERMIT.Web.Renderer
import           HERMIT.Web.Types

import           Network.HTTP.Types (Status, status200, status500)
import qualified Network.Wai as Wai

import           Web.Scotty.Trans

------------------------------- the plugin ------------------------------------
  
plugin :: Plugin
plugin = hermitPlugin $ \ phaseInfo -> if phaseNum phaseInfo == 0
                                       then scopedKernel . server
                                       else const return

-- | The meat of the plugin, which implements the actual Web API.
server :: [CommandLineOption] -> ScopedKernel -> SAST -> IO ()
server _opts skernel initSAST = do
    sync <- newTVarIO def

    let -- Functions required by Scotty to run our custom WebM monad.
        runWebM :: WebM a -> IO a
        runWebM m = do
            r <- flip runReaderT sync $ runErrorT $ runWebT m
            case r of
                Left err -> fail $ "Startup error: " ++ show err
                Right r' -> return r'

        runAction :: WebM Wai.Response -> IO Wai.Response
        runAction m = do
            r <- flip runReaderT sync $ runErrorT $ runWebT m
            case r of
                Left err -> handleError skernel err
                Right r' -> return r'

        -- Default state provided to every user that connects.
        initState = mkCLState skernel initSAST

    scottyT 3000 runWebM runAction $ do
        post "/connect"  $ connect initState
        post "/command"    command
        get  "/commands"   commands

-- | Build a default state for a new user.
mkCLState :: ScopedKernel -> SAST -> CommandLineState
mkCLState kernel sast =
    CommandLineState
        { cl_cursor         = sast
        , cl_pretty         = Clean.ppCoreTC
        , cl_pretty_opts    = def
        , cl_render         = unicodeConsole -- TODO: co-opt instead of doing getResult
        , cl_height         = 30
        , cl_nav            = False
        , cl_running_script = False
        , cl_tick           = error "cl_tick" -- TODO: debugging commands will hit this
        , cl_corelint       = False
        , cl_failhard       = False
        , cl_window         = mempty
        , cl_dict           = mkDict $ shell_externals ++ externals
        , cl_scripts        = []
        , cl_kernel         = kernel
        , cl_initSAST       = sast
        , cl_version        = VersionStore
                                { vs_graph = []
                                , vs_tags  = []
                                }
        }

-- | Turn WebAppError into a Response.
handleError :: ScopedKernel -> WebAppError -> IO Wai.Response
handleError k WAEAbort = do
    abortS k
    return $ msgBuilder "HERMIT Aborting" status200 
handleError k (WAEResume sast) = do
    resumeS k sast
    return $ msgBuilder "HERMIT Resuming" status200 
handleError _ (WAEError str) = return $ msgBuilder str status500 

-- | Turn a string and status into a Response containing a JSON-encoded Msg.
msgBuilder :: String -> Status -> Wai.Response
msgBuilder msg s = Wai.ResponseBuilder s [("Content-Type","application/json")]
                                     $ fromLazyByteString $ Aeson.encode $ Msg msg

------------------------- connecting a new user -------------------------------
  
connect :: CommandLineState -> ActionH ()
connect initState = do
    uid <- webm $ do sync <- ask
                     liftIO $ do
                        mvar <- newMVar initState
                        atomically $ do
                            st <- readTVar sync
                            let k = nextKey (users st)
                            writeTVar sync $ st { users = Map.insert k mvar (users st) }
                            return k
    json $ Token uid (cl_cursor initState)

-- | Generate the next user id.
nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

--------------------------- running a command ---------------------------------
  
command :: ActionH ()
command = do
    Command (Token u sast) cmd <- jsonData

    case parseScript cmd of
        Left  str    -> raise $ T.pack $ "Parse failure: " ++ str
        Right script -> evalStmts u (\st -> st { cl_cursor = sast }) script

    (glyphs, ast) <- clm u id getResult
    json $ CommandResponse glyphs ast

-- | evalStmts and evalExpr copied here so we can special case abort/resume.
evalStmts :: UserID -> (CommandLineState -> CommandLineState) -> Script -> ActionH ()
evalStmts u f = mapM_ (evalExpr u f)

evalExpr :: UserID -> (CommandLineState -> CommandLineState) -> ExprH -> ActionH ()
evalExpr u f expr = do
    dict <- clm u f $ State.gets cl_dict
    runKureM (\case
                 -- special case these so the MVar doesn't hang
                 MetaCommand Resume  -> clm u f (State.gets cl_cursor) >>= webm . throwError . WAEResume
                 MetaCommand Abort   -> webm $ throwError WAEAbort
                 KernelEffect effect -> clm u f $ performKernelEffect effect expr
                 ShellEffect effect  -> clm u f $ performShellEffect effect
                 QueryFun query      -> clm u f $ performQuery query
                 MetaCommand meta    -> clm u f $ performMetaCommand meta
             )
             (raise . T.pack)
             (interpExprH dict interpShellCommand expr)

-- | Pretty print the current AST using glyphs.
getResult :: MonadIO m => CLM m ([Glyph], SAST)
getResult = do
    st <- State.get
    focusPath <- getFocusPath
    let skernel = cl_kernel st
        ppOpts = (cl_pretty_opts st) { po_focus = Just focusPath }
    iokm2clm' "Rendering error: "
              (\doc -> let Glyphs gs = renderCode ppOpts doc in return (gs, cl_cursor st))
              (toASTS skernel (cl_cursor st) >>= \ ast ->
                queryK (kernelS skernel) ast (extractT $ pathT (cl_window st) $ liftPrettyH ppOpts $ cl_pretty st) (cl_kernel_env st))

-------------------------- get list of commands -------------------------------
  
commands :: ActionH ()
commands = json
         $ CommandList 
         $ [ CommandInfo (externName e)
                         (unlines $ externHelp e)
                         (externTags e)
           | e <- shell_externals ++ externals ]

