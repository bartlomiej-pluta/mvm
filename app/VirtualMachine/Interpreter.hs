module VirtualMachine.Interpreter where

import Data.Word (Word8)
import Data.List (intercalate)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.Except (throwError)
import Control.Monad.State (get, liftIO, lift, runState, evalState)
import qualified Data.Map as M
import qualified Data.ByteString as B

import VirtualMachine.VM (VM(..), Op, empty, pop, pushS, forward, getPc)
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

interpret :: VM -> [Unit] -> ExceptT String IO VM
interpret vm@VM { _halt = True} _ = except $ Right $ vm
interpret vm units = do
  vm' <- interpretUnit vm units
  interpret vm' units

interpretUnit :: VM -> [Unit] -> ExceptT String IO VM
interpretUnit _ [] = except $ Left "Nothing to interpret"
interpretUnit vm units
  | pc >= progSize = except $ Left $ "PC (=" ++ (show pc) ++ ") exceeds program size (=" ++ (show progSize) ++ ")"
  | otherwise = case unit of
      (Instr instr) -> dispatchInstr vm units instr
      (Byte _)      -> except $ Left $ "PC (=" ++ (show pc) ++ ") currently points to the data byte rather than instruction" 
  where
    pc = _pc vm
    progSize = length units
    unit = units !! pc

dispatchInstr :: VM -> [Unit] -> Instruction -> ExceptT String IO VM
dispatchInstr vm units instr = do
  liftIO $ debugPrint vm instr units

  case instr of 
    Simple {}  -> except $ interpretSimple vm units instr 
    Complex {} -> interpretComplex vm units instr

debugPrint :: VM -> Instruction -> [Unit] -> IO ()
debugPrint vm instr units = if _debug vm
           then do
             let pc = _pc vm
             let noParams = _noParams instr
             let params = intercalate "" $ map (show . _byte) $ take noParams $ drop (pc + 1) $ units
             putStrLn $ show vm
             putStrLn $ (show pc) ++ ": " ++ (show $ _op instr) ++ " " ++ params
             return ()
           else return ()

interpretSimple :: VM -> [Unit] -> Instruction -> Either String VM
interpretSimple vm units instr = flip evalState vm $ runExceptT $ do
  pc <- lift getPc
  let noParams = _noParams instr  
  let noPops = _noPops instr     
  let paramBytes = take noParams $ drop (pc + 1) $ units
  let params = map (fromIntegral . _byte) paramBytes   
  let action = _sAction instr
  pops <- lift $ pop noPops
  if length pops == noPops
  then do
    let pushes = action params pops
    lift $ pushS pushes
    lift $ forward $ noParams + 1
    vm' <- lift get
    return vm'
  else throwError $ "Attempt to pop from empty stack: tried to pop " ++ (show noPops) ++ " elements, got " ++ (show $ length pops)  

interpretComplex :: VM -> [Unit] -> Instruction -> ExceptT String IO VM
interpretComplex vm units instr = if length pops == noPops
  then action vm' params pops
  else throwError $ "Attempt to pop from empty stack: tried to pop " ++ (show noPops) ++ " elements, got " ++ (show $ length pops)   
  where    
    pc = _pc vm
    noParams = _noParams instr  
    noPops = _noPops instr  
 
    paramBytes = take noParams $ drop (pc + 1) $ units 
    params = map (fromIntegral . _byte) paramBytes     
    (pops, vm') = runState (pop noPops) vm

    action = _cAction instr
    
run :: VM -> B.ByteString -> ExceptT String IO VM
run vm code = (return $ B.unpack code) >>= (except . parse) >>= interpret vm

runEmpty :: B.ByteString -> ExceptT String IO VM
runEmpty = run empty