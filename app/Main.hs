module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified VirtualMachine as VM

import Assembler.Compiler (compile)


run :: String -> IO ()
run input = case compile input of
  (Right bytes) -> print $ VM.run VM.empty (B.pack bytes)
  (Left err) -> putStrLn err

main :: IO ()
main = do
  (filename:_) <- getArgs
  input <- readFile filename
  run input
