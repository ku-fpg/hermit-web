{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, LambdaCase, TupleSections #-}
module HERMIT.Web.Types where

import           Control.Applicative
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Concurrent.STM
#if MIN_VERSION_mtl(2,2,1)
import           Control.Monad.Except
#else
import           Control.Monad.Error
#endif
import           Control.Monad.Reader

import           Data.Default
import qualified Data.Map as Map
import           Data.Text.Lazy (Text)

import           HERMIT.Kernel.Scoped
import           HERMIT.Shell.Types hiding (clm)
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

data WebAppException = WAEAbort | WAEResume SAST | WAEError String
    deriving (Show)

#if !(MIN_VERSION_mtl(2,2,1))
instance Error WebAppException where strMsg = WAEError
#endif

#if MIN_VERSION_mtl(2,2,1)
newtype WebT m a = WebT { runWebT :: ExceptT WebAppException (ReaderT (TVar WebAppState) m) a }
#else
newtype WebT m a = WebT { runWebT :: ErrorT WebAppException (ReaderT (TVar WebAppState) m) a }
#endif
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (TVar WebAppState), MonadError WebAppException)

instance MonadTrans WebT where
    lift = WebT . lift . lift

type WebM = WebT IO

type ScottyH a = ScottyT Text WebM a
type ActionH a = ActionT Text WebM a

-- The monad transformer stack is quite ridiculous at this point.
-- So here are some helpers to get things to the right place.

view :: WebM WebAppState
view = ask >>= liftIO . readTVarIO

views :: (WebAppState -> b) -> WebM b
views f = view >>= return . f

viewUser :: UserID -> WebM (MVar CommandLineState, Chan (Either String [Glyph]))
viewUser u = views users >>= maybe (throwError $ WAEError "User Not Found") return . Map.lookup u

-- Do something in the CLM IO monad for a given user and state modifier.
clm :: MonadTrans t => UserID -> (CommandLineState -> CommandLineState) -> CLT IO a -> t WebM a
clm u f m = lift $ do
    mvar <- liftM fst $ viewUser u
    r <- liftIO $ do s <- takeMVar mvar
                     (r,s') <- runCLT (f s) m
                     let (s'',r') = either (\case CLAbort         -> (s , Left WAEAbort)
                                                  CLResume   sast -> (s', Left (WAEResume sast))
                                                  CLError    err  -> (s , Left (WAEError err))
                                                  CLContinue st   -> (st, Left (WAEError "continue not supported")))
                                           ((s',) . Right) r
                     putMVar mvar s''
                     return r'
    either throwError return r

-- Do something to the web application state.
webm :: MonadTrans t => WebM a -> t WebM a
webm = lift
