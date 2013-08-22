{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, LambdaCase, RankNTypes,
             GeneralizedNewtypeDeriving, FlexibleInstances, MultiParamTypeClasses #-}
module HERMIT.Web where

import GhcPlugins hiding ((<>), liftIO, text, display)

import HERMIT.Primitive.Composite
import HERMIT.Primitive.Navigation
import HERMIT.Primitive.Unfold
import HERMIT.Dictionary
import HERMIT.Interp
import HERMIT.Kure
import HERMIT.Parser
import HERMIT.Optimize
import HERMIT.Shell.Externals
import HERMIT.Shell.Types
import HERMIT.Web.JSON

import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.State.Lazy hiding (get, put)
import qualified Control.Monad.State.Lazy as State

import Data.Default
import qualified Data.Map as Map
import Data.Monoid
import qualified Data.Text.Lazy as T

import qualified Language.Haskell.TH as TH

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

type WebM = WebT OM

type ScottyH a = ScottyT WebM a
type ActionH a = ActionT WebM a

instance Show InterpState where
    show = show . isAST

plugin :: Plugin
plugin = optimize server

-- Global state is threaded with an MVar
-- importantly, the web app code below can pretend
-- there is a normal state monad.
mkScottyApp :: WebAppState -> ScottyH () -> OM Wai.Application
mkScottyApp wst defs = do
    sync <- liftIO newEmptyMVar
    let runWebM :: WebM a -> OM a
        runWebM m = do
            (r,w) <- runStateT (runWebT m) wst
            h <- State.get
            liftIO $ putMVar sync (h,w)
            return r
        runToIO :: WebM a -> IO a
        runToIO m = do
            (h,w) <- liftIO $ takeMVar sync
            ((r,w'),h') <- omToIO h $ runStateT (runWebT m) w
            liftIO $ putMVar sync (h',w')
            return r
    scottyApp runWebM runToIO defs

-- The monad transformer stack is quite ridiculous at this point.
-- So here are some helpers to get things to the right place.

-- Do something in the OM monad (the HERMIT plugin monad, see HERMIT.Optimize)
om :: MonadTrans t => OM a -> t WebM a
om = lift . lift

-- Do something to the web application state.
webm :: MonadTrans t => WebM a -> t WebM a
webm = lift

server :: [CommandLineOption] -> OM ()
server _opts = firstPhase $ do
  app <- mkScottyApp def $ do
        om $ run $ tryR simplifyR
        om $ run $ tryR simplifyR
        om $ run $ tryR simplifyR
        om $ run $ tryR simplifyR
        webm $ State.put $ def { users = Map.singleton 3 5 }

        get "/" $ do
            om $ run $ onebuR $ promoteExprR unfoldR
            st'' <- om State.get
            om $ at (liftM snocPathToPath $ rhsOf $ TH.mkName "main") display
            text $ showText st''

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



{-
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
-}

  liftIO $ Warp.run 3000 app

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
                 KernelEffect effect -> performKernelEffect effect
                 ShellEffect effect  -> performShellEffect effect
                 QueryFun query      -> performQuery query
                 MetaCommand meta    -> performMetaCommand meta
             )
             (raise . T.pack)
             (interpExprH dict interpShellCommand expr)

{-
ata KernelEffect :: * where
   Apply      :: (Injection GHC.ModGuts g, Walker HermitC g) => RewriteH g              -> KernelEffect
   Pathfinder :: (Injection GHC.ModGuts g, Walker HermitC g) => TranslateH g LocalPathH -> KernelEffect
   Direction  ::                                                Direction               -> KernelEffect
   BeginScope ::                                                                           KernelEffect
   EndScope   ::                                                                           KernelEffect
   CorrectnessCritera :: (Injection GHC.ModGuts g, Walker HermitC g) => TranslateH g () -> KernelEffect
-}
performKernelEffect :: KernelEffect -> ActionM ()
performKernelEffect _ = raise "performKernelEffect unimplemented"
{-
performKernelEffect (Apply rr) = do
    sast' <- iokm2clm "Rewrite failed: " $ applyS sk (cl_cursor st) rr kEnv
-}
performShellEffect :: ShellEffect -> ActionM ()
performShellEffect _ = raise "performShellEffect unimplemented"
performQuery :: QueryFun -> ActionM ()
performQuery _ = raise "performShellEffect unimplemented"
performMetaCommand :: MetaCommand -> ActionM ()
performMetaCommand _ = raise "performShellEffect unimplemented"
