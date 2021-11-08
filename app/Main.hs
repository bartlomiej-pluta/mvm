module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified VirtualMachine as VM

import Assembler.Compiler (compile)

import Control.Monad.Trans.Except


run :: String -> IO ()
run input = case compile input of
  (Right bytes) -> runExceptT (VM.run VM.empty (B.pack bytes)) >>= print >> return ()
  (Left err) -> putStrLn err

main :: IO ()
main = do
  (filename:_) <- getArgs
  input <- readFile filename
  run input
