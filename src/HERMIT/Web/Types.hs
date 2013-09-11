{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}
module HERMIT.Web.Types where

import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Default
import qualified Data.Map as Map

import           HERMIT.Kernel.Scoped
import           HERMIT.Shell.Types
import           HERMIT.Web.JSON

import           Web.Scotty.Trans

-- | A note about the design here:
--
-- The WebM monad uses a 'ReaderT (TVar WebAppState)' rather
-- than 'StateT WebAppState' so Scotty actions are non-blocking.
-- Using a state transformer requires global state to be synchronized
-- with an MVar, meaning every request blocks. By storing a TVar
-- in a reader, only requests that modify the TVar block.
--
-- Additionally, the map held by the TVar maps users to MVars
-- containing their own CommandLineState. This is done for two reasons:
--
--   1. Calls to /command won't block, as the TVar doesn't need to be
--      changed. Currently, only /connect blocks as it adds to the map.
--
--   2. Calls to /command by the _same user_ will block each other,
--      allowing commands to complete in order. See defn of 'clm' below.
newtype WebAppState = WebAppState { users :: Map.Map UserID (MVar CommandLineState, Chan (Either String [Glyph])) }

instance Default WebAppState where
    def = WebAppState { users = Map.empty }

data WebAppError = WAEAbort | WAEResume SAST | WAEError String
    deriving (Show)

instance Error WebAppError where strMsg = WAEError

newtype WebT m a = WebT { runWebT :: ErrorT WebAppError (ReaderT (TVar WebAppState) m) a }
    deriving (Monad, MonadIO, MonadReader (TVar WebAppState), MonadError WebAppError)

instance MonadTrans WebT where
    lift = WebT . lift . lift

type WebM = WebT IO

type ScottyH a = ScottyT WebM a
type ActionH a = ActionT WebM a

-- The monad transformer stack is quite ridiculous at this point.
-- So here are some helpers to get things to the right place.

view :: WebM WebAppState
view = ask >>= liftIO . readTVarIO

views :: (WebAppState -> b) -> WebM b
views f = view >>= return . f

viewUser :: UserID -> WebM (MVar CommandLineState, Chan (Either String [Glyph]))
viewUser u = views users >>= maybe (throwError $ WAEError "User Not Found") return . Map.lookup u

-- Do something in the CLM IO monad for a given user and state modifier.
clm :: MonadTrans t => UserID -> (CommandLineState -> CommandLineState) -> CLM IO a -> t WebM a
clm u f m = lift $ do
    mvar <- liftM fst $ viewUser u
    r <- liftIO $ do s <- takeMVar mvar
                     (r,s') <- runCLMToIO (f s) m
                     putMVar mvar s'
                     return r
    either (throwError . WAEError) return r

-- Do something to the web application state.
webm :: MonadTrans t => WebM a -> t WebM a
webm = lift
