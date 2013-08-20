{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase #-}
module HERMIT.Web where

import GhcPlugins hiding ((<>), liftIO)

import HERMIT.Kernel.Scoped
import HERMIT.Dictionary
import HERMIT.Interp
import HERMIT.Parser
-- import HERMIT.Optimize
import HERMIT.Plugin
import HERMIT.Shell.Externals
import HERMIT.Shell.Types
import HERMIT.Web.JSON

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)

import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text.Lazy as T

import Language.KURE

import Web.Scotty

plugin :: Plugin
plugin = hermitPlugin server
{-
plugin = optimize server

server :: [CommandLineOption] -> OM ()
server _opts = firstPhase $ liftIO $ do
-}
server :: PhaseInfo -> [CommandLineOption] -> ModGuts -> CoreM ModGuts
server _pi _opts = scopedKernel $ \ _kernel _initSAST -> do
    -- unique to token mapping
    users <- newMVar (Map.empty :: Map.Map Integer Integer)

    scotty 3000 $ do
        get "/" $ do
            html $ mconcat ["<html><body>"
                           ,"Hackers Interface:</br>"
                           -- abort
                           ,"<form method=POST action=\"/abort\">"
                           ,"<input type=submit name=Go/>"
                           ,"</form>"
                           -- resume
                           ,"<form method=POST action=\"/resume\">"
                           ,"<input type=submit name=Go/>"
                           ,"</form>"
                           ,"</body></html>"]

        post "/init" $ do
            m <- liftIO $ takeMVar users
            let k = nextKey m
            liftIO $ putMVar users $ Map.insert k 0 m
            json $ Token k 0

        post "/command" $ do
            Command (Token u t) cmd path <- jsonData
            token <- checkToken u t users

            case parseScript cmd of
                Left  str    -> raise $ T.pack $ "Parse failure: " ++ str
                Right script -> evalStmts script

            json $ CommandResponse token "" path

{-
        get "/commands" $ do
            json $ mconcat []

        get "/complete" $ do
            query <- jsonData
            json $ mconcat []
-}

        get "/reset" $ do
            t <- jsonData
            json $ t { tToken = 0 }

{-# INLINE showText #-}
showText :: Show a => a -> T.Text
showText = T.pack . show


type UniqueId = Integer
type TokenNum = Integer
type UserDB = MVar (Map.Map UniqueId TokenNum)

nextKey :: Map.Map Integer a -> Integer
nextKey m | Map.null m = 0
          | otherwise = let (k,_) = Map.findMax m in k + 1

checkToken :: Integer -> Integer -> UserDB -> ActionM Token
checkToken u t db = do
    m <- liftIO $ takeMVar db
    t' <- maybe (raise "user id not found!") return $ Map.lookup u m
    guardMsg (t >= t') "token out of order"
    liftIO $ putMVar db $ Map.adjust (+1) u m
    return $ Token u (t'+1)

evalStmts :: Script -> ActionM ()
evalStmts = mapM_ evalExpr

evalExpr :: ExprH -> ActionM ()
evalExpr expr = do
    let dict = mkDict shell_externals
    runKureM (\case
                 KernelEffect effect -> performKernelEffect effect expr
                 ShellEffect effect  -> performShellEffect effect
                 QueryFun query      -> performQuery query
                 MetaCommand meta    -> performMetaCommand meta
             )
             (raise . T.pack)
             (interpExprH dict interpShellCommand expr)

performKernelEffect :: KernelEffect -> ExprH -> ActionM ()
performKernelEffect = undefined
performShellEffect :: ShellEffect -> ActionM ()
performShellEffect = undefined
performQuery :: QueryFun -> ActionM ()
performQuery = undefined
performMetaCommand :: MetaCommand -> ActionM ()
performMetaCommand = undefined
