module Main where

import System.Environment (getArgs)

import Runner (run)

main :: IO ()
main = do
  (filename:_) <- getArgs
  input <- readFile filename
  result <- run input
  case result of
    (Right vm) -> do
      putStrLn $ "\nDone\n" ++ show vm
    (Left err) -> putStrLn $ "\n\nError:\n" ++ err
