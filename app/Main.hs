module Main where

import System.Environment
import Control.Monad.Trans.Except (runExceptT, except)

import VirtualMachine.VM (VM)
import VirtualMachine.Interpreter (run)
import Assembler.Compiler (compile)
import qualified Data.ByteString as B

interpret :: String -> IO (Either String VM)
interpret input = runExceptT $ (except $ return $ input) >>= (except . compile) >>= (except . return . B.pack) >>= run
  
main :: IO ()
main = do
  (filename:_) <- getArgs
  input <- readFile filename
  result <- interpret input
  case result of
    (Right vm) -> do
      putStrLn $ "\n\nDone:\n" ++ (show vm)
    (Left err) -> putStrLn $ "\n\nError:\n" ++ err
