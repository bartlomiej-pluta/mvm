module Main where

import System.Environment

import Runner (run, runDebug)

main :: IO ()
main = do
  (filename:_) <- getArgs
  input <- readFile filename
  result <- run input
  case result of
    (Right vm) -> do
      putStrLn $ "\n\nDone:\n" ++ (show vm)
    (Left err) -> putStrLn $ "\n\nError:\n" ++ err
