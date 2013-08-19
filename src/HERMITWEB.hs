{-# LANGUAGE OverloadedStrings #-}
module HERMITWEB where

import GhcPlugins hiding ((<>), liftIO)

import HERMIT.Kernel.Scoped
import HERMIT.Plugin

import Control.Monad.IO.Class (liftIO)
import Data.Char (isDigit)
import Data.Monoid
import qualified Data.Text.Lazy as T
import Web.Scotty

{-# INLINE showText #-}
showText :: Show a => a -> T.Text
showText = T.pack . show

plugin :: Plugin
plugin = hermitPlugin server

server :: PhaseInfo -> [CommandLineOption] -> ModGuts -> CoreM ModGuts
server _pi _opts = scopedKernel $ \ kernel initSAST -> do

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
                           ,"<input type=text name=sast value=\"" <> showText initSAST <> "\"/>"
                           ,"<input type=submit name=Go/>"
                           ,"</form>"
                           ,"</body></html>"]

        post "/abort" $ do
            liftIO $ abortS kernel :: ActionM () -- liftIO is underconstrained without the type sig

        post "/resume" $ do
            sast <- param "sast"
            liftIO $ resumeS kernel sast

instance Parsable SAST where
    parseParam t = if all isDigit str
                   then Right $ SAST (read str)
                   else Left "cannot parse SAST"
        where str = dropWhile (not . isDigit) $ T.unpack t

