{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module HERMIT.Web.Actions
    ( connect
    , command
    , commands
    , history
    , complete
    ) where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Error
import           Control.Monad.Reader
import qualified Control.Monad.State.Lazy as State

import           Data.Char (isSpace)
import           Data.Default
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid

import           HERMIT.Dictionary
import           HERMIT.External
import           HERMIT.Kure
import           HERMIT.Parser
import qualified HERMIT.PrettyPrinter.Clean as Clean
import           HERMIT.Shell.Command
import           HERMIT.Shell.Dictionary
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

    ast <- clm u (\st -> st { cl_cursor = sast }) $ evalScript cmd >> State.gets cl_cursor

    es <- webm $ liftM snd (viewUser u) >>= liftIO . getUntilEmpty
    let (ms,gs) = partitionEithers es
    json $ CommandResponse (optionalMsg ms) (optionalAST gs) ast

optionalAST :: [[Glyph]] -> Maybe [Glyph]
optionalAST [] = Nothing
optionalAST gs = Just (last gs)

optionalMsg :: [String] -> Maybe String
optionalMsg [] = Nothing
optionalMsg ss = Just (unlines ss)

getUntilEmpty :: Chan a -> IO [a]
getUntilEmpty chan = ifM (isEmptyChan chan)
                         (return [])
                         (readChan chan >>= flip liftM (getUntilEmpty chan) . (:))

-------------------------- get list of commands -------------------------------

-- TODO: get per-user list of commands
commands :: ActionH ()
commands = json
         $ CommandList
           [ CommandInfo (externName e)
                         (unlines $ externHelp e)
                         (externTags e)
                         (externTypeString e)
           | e <- shell_externals ++ externals ]

-------------------------- get version history --------------------------------

history :: ActionH ()
history = do
    Token u _ <- jsonData
    v <- clm u id $ State.gets cl_version
    json $ History [ HCmd from (unparseExprH e) to | (from,e,to) <- vs_graph v ]
                   [ HTag str ast | (str,ast) <- vs_tags v ]

---------------------------- get completions ----------------------------------

complete :: ActionH ()
complete = do
    Complete u cmd <- jsonData
    mvar <- liftM fst $ webm $ viewUser u
    let (rCmd,rPrev) = break isSpace $ reverse cmd
    res <- liftIO $ shellComplete mvar rPrev $ reverse rCmd
    json $ Completions res
