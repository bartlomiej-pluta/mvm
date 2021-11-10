module Runner where

import Control.Monad.Trans.Except (runExceptT, except)
import qualified Data.ByteString as B

import Assembler.Compiler (compile)
import VirtualMachine.VM (VM(..), empty)
import qualified VirtualMachine.Interpreter as VM (run)

run :: String -> IO (Either String VM)
run input = runExceptT $ (except $ return $ input) >>= (except . compile) >>= (except . return . B.pack) >>= VM.run empty

runDebug :: String -> IO (Either String VM)
runDebug input = runExceptT $ (except $ return $ input) >>= (except . compile) >>= (except . return . B.pack) >>= VM.run empty { _debug = True }