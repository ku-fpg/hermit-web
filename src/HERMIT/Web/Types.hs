{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module HERMIT.Web.Types where

import HERMIT.Shell.Types
import HERMIT.Kernel.Scoped

import Control.Monad.Error
import Control.Monad.State.Lazy hiding (get, put)

import Data.Default
import qualified Data.Map as Map

import Web.Scotty.Trans

newtype WebAppState = WebAppState { users :: Map.Map Integer Integer }
    deriving (Show)

instance Default WebAppState where
    def = WebAppState { users = Map.empty }

data WebAppError = WAEAbort | WAEResume SAST | WAEError String

instance Error WebAppError where strMsg = WAEError

newtype WebT m a = WebT { runWebT :: ErrorT WebAppError (StateT WebAppState m) a }
    deriving (Monad, MonadIO, MonadState WebAppState, MonadError WebAppError)

instance MonadTrans WebT where
    lift = WebT . lift . lift

type WebM = WebT (CLM IO)

type ScottyH a = ScottyT WebM a
type ActionH a = ActionT WebM a

-- The monad transformer stack is quite ridiculous at this point.
-- So here are some helpers to get things to the right place.

-- Do something in the CLM IO monad (the HERMIT plugin monad, see HERMIT.Optimize)
clm :: MonadTrans t => CLM IO a -> t WebM a
clm = lift . lift

-- Do something to the web application state.
webm :: MonadTrans t => WebM a -> t WebM a
webm = lift

type UniqueId = Integer
type TokenNum = Integer

