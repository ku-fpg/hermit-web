module Main where

import HERMIT.Driver

import System.Environment
import System.Process
import System.Exit

main :: IO ()
main = do
    (file_nm:_) <- getArgs -- TODO: real option parser
    putStrLn $ "[starting " ++ hermit_version ++ " web service on " ++ file_nm ++ "]"

    let cmds = file_nm : ghcFlags ++ plugin_cmds
        plugin_cmds = ["-fplugin=HERMIT.Web"
                      ,"-package ghc"]
    putStrLn $ "% ghc " ++ unwords cmds
    ex <- runWithArgs "ghc" cmds
    exitWith ex

runWithArgs :: String -> [String] -> IO ExitCode
runWithArgs cmd args = do
    (_,_,_,r) <- createProcess $ proc cmd args
    waitForProcess r
