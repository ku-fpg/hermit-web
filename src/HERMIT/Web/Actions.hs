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
import           Data.Either
import qualified Data.Map as Map
import           Data.Monoid

import           HERMIT.Dictionary
import           HERMIT.External
import           HERMIT.Kernel.Scoped
import           HERMIT.Kure
import           HERMIT.Parser

import           HERMIT.Plugin
import           HERMIT.Plugin.Builder
import           HERMIT.Plugin.Types

import           HERMIT.PrettyPrinter.Common (po_width)

import           HERMIT.Shell.Command
import           HERMIT.Shell.Dictionary
import           HERMIT.Shell.Externals
import           HERMIT.Shell.Types hiding (clm)

import           HERMIT.Web.JSON
import           HERMIT.Web.Renderer
import           HERMIT.Web.Types

import           Web.Scotty.Trans

------------------------- connecting a new user -------------------------------

connect :: PhaseInfo -> ScopedKernel -> SAST -> ActionH ()
connect phaseInfo kernel sast = do
    uid <- webm $ do sync <- ask
                     liftIO $ do
                        chan <- newChan
                        cls <- mkCLState chan phaseInfo kernel sast
                        mvar <- newMVar cls
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
mkCLState :: Chan (Either String [Glyph]) -> PhaseInfo -> ScopedKernel -> SAST -> IO CommandLineState
mkCLState chan phaseInfo kernel sast = do
    ps <- defPS sast kernel phaseInfo
    return $ CommandLineState
                { cl_pstate = ps { ps_render = webChannel chan }
                , cl_height         = 30
                , cl_nav            = False
                , cl_window         = mempty
                , cl_dict           = mkDict $ shell_externals ++ externals
                , cl_lemmas         = []
                , cl_scripts        = []
                , cl_initSAST       = sast
                , cl_version        = VersionStore
                                        { vs_graph = []
                                        , vs_tags  = []
                                        }
                }

--------------------------- running a command ---------------------------------

command :: ActionH ()
command = do
    Command (Token u sast) cmd mWidth <- jsonData

    let changeState st = let st' = maybe st (\w -> setPrettyOpts st ((cl_pretty_opts st) { po_width = w })) mWidth
                         in setCursor st' sast
        
    ast <- clm u changeState $ evalScript cmd >> State.gets cl_cursor

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
           [ CommandInfo (externName e) (unlines $ externHelp e) (externTags e) aTys rTy
           | e <- shell_externals ++ externals
           , let (aTys, rTy) = externTypeArgResString e ]

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
