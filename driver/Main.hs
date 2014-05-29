{-# LANGUAGE LambdaCase #-}
module Main where

import HERMIT.Driver

import System.Environment

import Data.List
import Data.List.Split
import Data.Version
import Paths_hermit_web as P

hermitWebVersion :: String
hermitWebVersion = "HERMIT-Web v" ++ showVersion P.version

hermitWebUsage :: IO ()
hermitWebUsage = mapM_ putStrLn [hermitWebVersion, "", replace "hermit" "hermit-web" usageOutput]
    where replace :: String -> String -> String -> String
          replace old new = intercalate new . splitOn old

main :: IO ()
main = getArgs >>= \case
    (file_nm:rest) -> do
        putStrLn $ "[starting " ++ hermitWebVersion ++ " on " ++ file_nm ++ "]"
        hermitDriver $ file_nm : "-opt=HERMIT.Web" : rest
    [] -> hermitWebUsage
