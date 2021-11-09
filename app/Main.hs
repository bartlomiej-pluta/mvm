module Main where

import System.Environment
import qualified Data.ByteString as B
import qualified VirtualMachine as VM

import Assembler.Compiler (compile)

import Control.Monad.Trans.Except


run :: String -> IO (Either String VM.VM)
run input = runExceptT $ (except $ return $ input) >>= (except . compile) >>= (except . return . B.pack) >>= VM.run
  
main :: IO ()
main = do
  (filename:_) <- getArgs
  input <- readFile filename
  result <- run input
  case result of
    (Right vm) -> do
      putStrLn $ "\n\nDone:\n" ++ (show vm)
    (Left err) -> putStrLn $ "\n\nError:\n" ++ err
