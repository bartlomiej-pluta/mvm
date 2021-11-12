module VirtualMachine.Interpreter where

import Data.Word (Word8)
import Data.List (intercalate)

import Control.Monad.Trans.State (get, evalStateT)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Control.Monad.State (liftIO)
import qualified Data.Map as M
import qualified Data.ByteString as B

import VirtualMachine.VM (VM(..), Op, Machine, pop, pushS, forward, getPc, isHalted, isDebug)
import VirtualMachine.Instruction (Instruction(..), Unit(..), instructionByOp)


parseInstr :: [Word8] -> Either String (Instruction, [Word8])
parseInstr (opCode:rest) = do
  let op = toEnum . fromIntegral $ opCode :: Op
  instr <- case M.lookup op instructionByOp of
    (Just i) -> Right i
    Nothing -> Left "Unknown instruction"
  let noParams = _noParams instr
  let params = map fromIntegral $ take noParams rest :: [Word8]
  if length params == noParams 
  then return (instr, params)
  else Left $ "Expected " ++ (show noParams) ++ " parameter(s), got " ++ (show $ length params) ++ " for operator '" ++ (show op) ++ "'"
parseInstr [] = Left "Unexpected end of the file"

parse :: [Word8] -> Either String [Unit]
parse [] = Right []
parse code = do
  (instr, params) <- parseInstr code
  let paramBytes = map Byte params
  let noParams = _noParams instr
  rest <- parse (drop (noParams + 1) code)
  return $ [Instr instr] ++ paramBytes ++ rest

interpret :: [Unit] -> ExceptT String Machine ()
interpret units = do
  halted <- lift isHalted
  if halted 
  then return ()
  else do
    interpretUnit units
    interpret units
  
interpretUnit :: [Unit] -> ExceptT String Machine ()
interpretUnit [] = throwError "Nothing to interpret"
interpretUnit units = do
  pc <- lift getPc
  let progSize = length units
  if pc < progSize
  then case units !! pc of
    (Instr instr) -> dispatchInstr units instr
    (Byte _)      -> throwError $ "PC (=" ++ (show pc) ++ ") currently points to the data byte rather than instruction" 
  else throwError $ "PC (=" ++ (show pc) ++ ") exceeds program size (=" ++ (show progSize) ++ ")"

dispatchInstr :: [Unit] -> Instruction -> ExceptT String Machine ()
dispatchInstr units instr = do
  debug <- lift isDebug
  
  if debug
  then lift $ do
    vm <- get
    pc <- getPc
    let noParams = _noParams instr
    let params = intercalate "" $ map (show . _byte) $ take noParams $ drop (pc + 1) $ units
    liftIO $ putStrLn $ show vm
    liftIO $ putStrLn $ (show pc) ++ ": " ++ (show $ _op instr) ++ " " ++ params
    return ()
  else return ()

  case instr of
    Simple {}  -> interpretSimple units instr
    Complex {} -> interpretComplex units instr

interpretSimple :: [Unit] -> Instruction -> ExceptT String Machine ()
interpretSimple units instr = do
  pc <- lift getPc
  let noParams = _noParams instr  
  let noPops = _noPops instr     
  let paramBytes = take noParams $ drop (pc + 1) $ units
  let params = map (fromIntegral . _byte) paramBytes   
  let action = _sAction instr
  pops <- lift $ pop noPops  
  if length pops == noPops
  then lift $ do
    let pushes = action params pops
    pushS pushes
    forward $ noParams + 1
    return ()
  else throwError $ "Attempt to pop from empty stack: tried to pop " ++ (show noPops) ++ " elements, got " ++ (show $ length pops)  

interpretComplex :: [Unit] -> Instruction -> ExceptT String Machine ()
interpretComplex units instr = do
  pc <- lift getPc
  let noParams = _noParams instr  
  let noPops = _noPops instr     
  let paramBytes = take noParams $ drop (pc + 1) $ units
  let params = map (fromIntegral . _byte) paramBytes   
  let action = _cAction instr
  pops <- lift $ pop noPops  
  if length pops == noPops
  then do action params pops
  else throwError $ "Attempt to pop from empty stack: tried to pop " ++ (show noPops) ++ " elements, got " ++ (show $ length pops)  

run :: VM -> B.ByteString -> IO (Either String VM)
run vm input = evalStateT (runExceptT machine) vm
  where machine = (return input) >>= (return .B.unpack) >>= (except . parse) >>= interpret >> (lift get)