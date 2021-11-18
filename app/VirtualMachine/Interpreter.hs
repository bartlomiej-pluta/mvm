module VirtualMachine.Interpreter where

import Data.Word (Word8)
import Data.List (intercalate)

import Control.Monad (when, unless)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.Trans.Except (except, runExceptT)
import Control.Monad.Except (throwError)
import Control.Monad.State (liftIO)
import qualified Data.Map as M
import qualified Data.ByteString as B

import VirtualMachine.VM (VM(..), Op, Computation, get, pop, pushS, forward, getPc, isHalted, isDebug)
import VirtualMachine.Instruction (Instruction(..), Unit(..), instructionByOp)


parseInstr :: [Word8] -> Either String (Instruction, [Word8])
parseInstr (opCode:rest) = do
  let op = toEnum . fromIntegral $ opCode :: Op
  instr <- case M.lookup op instructionByOp of
    (Just i) -> Right i
    Nothing -> Left "Unknown instruction"
  let noParams = _noParams instr
  let params = map fromIntegral $ take noParams rest :: [Word8]
  unless (length params == noParams) (Left $ "Expected " ++ show noParams ++ " parameter(s), got " ++ show (length params) ++ " for operator '" ++ show op ++ "'")
  return (instr, params)
parseInstr [] = Left "Unexpected end of the file"

parse :: [Word8] -> Either String [Unit]
parse [] = Right []
parse code = do
  (instr, params) <- parseInstr code
  let paramBytes = map Byte params
  let noParams = _noParams instr
  rest <- parse (drop (noParams + 1) code)
  return $ [Instr instr] ++ paramBytes ++ rest

interpret :: [Unit] -> Computation ()
interpret units = isHalted >>= \halted -> unless halted $ interpretUnit units >> interpret units

interpretUnit :: [Unit] -> Computation ()
interpretUnit [] = throwError "Nothing to interpret"
interpretUnit units = do
  pc <- getPc
  let progSize = length units
  unless (pc < progSize) (throwError $ "PC (=" ++ show pc ++ ") exceeds program size (=" ++ show progSize ++ ")")
  case units !! pc of
    (Instr instr) -> dispatchInstr units instr
    (Byte _)      -> throwError $ "PC (=" ++ show pc ++ ") currently points to the data byte rather than instruction"

dispatchInstr :: [Unit] -> Instruction -> Computation ()
dispatchInstr units instr = do
  debug <- isDebug

  when debug $ do
    vm <- get
    pc <- getPc
    let noParams = _noParams instr
    let params = intercalate "" $ map (show . _byte) $ take noParams $ drop (pc + 1) units
    liftIO $ print vm
    liftIO $ putStrLn $ show pc ++ ": " ++ show (_op instr) ++ " " ++ params

  case instr of
    Simple {}  -> interpretSimple units instr
    Complex {} -> interpretComplex units instr

interpretSimple :: [Unit] -> Instruction -> Computation ()
interpretSimple units instr = do
  pc <- getPc
  let noParams = _noParams instr
  let noPops = _noPops instr
  let paramBytes = take noParams $ drop (pc + 1) units
  let params = map (fromIntegral . _byte) paramBytes
  let action = _sAction instr
  pops <- pop noPops
  unless (length pops == noPops) (throwError $ "Attempt to pop from empty stack: tried to pop " ++ show noPops ++ " elements, got " ++ show (length pops))
  let pushes = action params pops
  pushS pushes
  forward $ noParams + 1

interpretComplex :: [Unit] -> Instruction -> Computation ()
interpretComplex units instr = do
  pc <- getPc
  let noParams = _noParams instr
  let noPops = _noPops instr
  let paramBytes = take noParams $ drop (pc + 1) units
  let params = map (fromIntegral . _byte) paramBytes
  let action = _cAction instr
  pops <- pop noPops
  unless (length pops == noPops) (throwError $ "Attempt to pop from empty stack: tried to pop " ++ show noPops ++ " elements, got " ++ show (length pops))
  action params pops

run :: VM -> B.ByteString -> IO (Either String VM)
run vm input = evalStateT (runExceptT machine) vm
  where machine = (except . parse . B.unpack) input >>= interpret >> get
