module Main where

import HERMIT.Driver

import System.Environment

import Data.Version
import Paths_hermit_web as P

hermitWebVersion :: String
hermitWebVersion = "HERMIT Web v" ++ showVersion P.version

main :: IO ()
main = do
    (file_nm:rest) <- getArgs 
    putStrLn $ "[starting " ++ hermitWebVersion ++ " on " ++ file_nm ++ "]"

    hermitDriver $ file_nm : "-opt=HERMIT.Web" : rest
