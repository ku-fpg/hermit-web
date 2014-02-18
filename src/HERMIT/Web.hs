{-# LANGUAGE OverloadedStrings, LambdaCase #-}
module HERMIT.Web (plugin) where

import           Blaze.ByteString.Builder (fromLazyByteString)

import           Control.Concurrent.STM
import           Control.Monad.Error
import           Control.Monad.Reader

import qualified Data.Aeson as Aeson
import           Data.Default

import           HERMIT.GHC hiding ((<>), liftIO)
import           HERMIT.Plugin.Builder
import           HERMIT.Kernel.Scoped

import           HERMIT.Web.Actions
import           HERMIT.Web.JSON
import           HERMIT.Web.Types

import           Network.HTTP.Types (Status, status200, status500)
import qualified Network.Wai as Wai
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)

import           Web.Scotty.Trans

------------------------------- the plugin ------------------------------------

plugin :: Plugin
plugin = buildPlugin $ \ phaseInfo -> if phaseNum phaseInfo == 0
                                      then scopedKernel . server phaseInfo
                                      else const return

-- | The meat of the plugin, which implements the actual Web API.
server :: PhaseInfo -> [CommandLineOption] -> ScopedKernel -> SAST -> IO ()
server phaseInfo _opts skernel initSAST = do
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

    scottyT 3000 runWebM runAction $ do
        middleware logStdoutDev
        post "/connect"  $ connect phaseInfo skernel initSAST
        post "/command"    command
        get  "/commands"   commands
        post "/history"    history
        post "/complete"   complete

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
msgBuilder msg s = Wai.responseBuilder s [("Content-Type","application/json")]
                                     $ fromLazyByteString $ Aeson.encode $ Msg msg

