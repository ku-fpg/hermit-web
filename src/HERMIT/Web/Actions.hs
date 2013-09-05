{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module HERMIT.Web.Actions (connect, command, commands) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State

import           Data.Default
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid
import qualified Data.Text.Lazy as T

import           HERMIT.Dictionary
import           HERMIT.External
import           HERMIT.Interp
import           HERMIT.Kure
import           HERMIT.Parser
import qualified HERMIT.PrettyPrinter.Clean as Clean
import           HERMIT.Shell.Command
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types
import           HERMIT.Kernel.Scoped

import           HERMIT.Web.JSON
import           HERMIT.Web.Renderer
import           HERMIT.Web.Types

import           Web.Scotty.Trans

------------------------- connecting a new user -------------------------------

connect :: ScopedKernel -> SAST -> ActionH ()
connect kernel sast = do
    uid <- webm $ do sync <- ask
                     liftIO $ do
                        chan <- newChan
                        mvar <- newMVar $ mkCLState chan kernel sast
                        atomically $ do
                            st <- readTVar sync
                            let k = nextKey (users st)
                            writeTVar sync $ st { users = Map.insert k (mvar,chan) (users st) }
                            return k
    json $ Token uid sast

-- | Generate the next user id.
nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

-- | Build a default state for a new user.
mkCLState :: Chan (Either String [Glyph]) -> ScopedKernel -> SAST -> CommandLineState
mkCLState chan kernel sast =
    CommandLineState
        { cl_cursor         = sast
        , cl_pretty         = Clean.ppCoreTC
        , cl_pretty_opts    = def
        , cl_render         = webChannel chan
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

--------------------------- running a command ---------------------------------

command :: ActionH ()
command = do
    Command (Token u sast) cmd <- jsonData

    ast <- case parseScript cmd of
        Left  str    -> raise $ T.pack $ "Parse failure: " ++ str
        Right script -> evalStmts u (\st -> st { cl_cursor = sast }) script

    es <- webm $ liftM snd (viewUser u) >>= liftIO . getChanContents
    let (ms,gs) = partitionEithers es
    when (null gs) $ raise "command did not give back an AST!"
    json $ CommandResponse (Msg $ unlines ms) (last gs) ast

-- | evalStmts and evalExpr copied here so we can special case abort/resume.
evalStmts :: UserID -> (CommandLineState -> CommandLineState) -> Script -> ActionH SAST
evalStmts _ _ [] = raise "No commands to evaluate!"
evalStmts u f s  = liftM last $ mapM (evalExpr u f) s

evalExpr :: UserID -> (CommandLineState -> CommandLineState) -> ExprH -> ActionH SAST
evalExpr u f expr = do
    dict <- clm u f $ State.gets cl_dict
    runKureM (\case
                 -- special case these so the MVar doesn't hang
                 MetaCommand Resume  -> clm u f (State.gets cl_cursor) >>= webm . throwError . WAEResume
                 MetaCommand Abort   -> webm $ throwError WAEAbort
                 KernelEffect effect -> clm u f $ performKernelEffect effect expr >> State.gets cl_cursor
                 ShellEffect effect  -> clm u f $ performShellEffect effect >> State.gets cl_cursor
                 QueryFun query      -> clm u f $ performQuery query >> State.gets cl_cursor
                 MetaCommand meta    -> clm u f $ performMetaCommand meta >> State.gets cl_cursor
             )
             (raise . T.pack)
             (interpExprH dict interpShellCommand expr)

{-
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
-}

-------------------------- get list of commands -------------------------------

commands :: ActionH ()
commands = json
         $ CommandList
         $ [ CommandInfo (externName e)
                         (unlines $ externHelp e)
                         (externTags e)
           | e <- shell_externals ++ externals ]

