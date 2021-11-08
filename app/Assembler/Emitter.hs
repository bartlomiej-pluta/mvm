module Assembler.Emitter where

import Control.Monad.State
import Data.Word (Word8)

import qualified Data.Map as M

import Assembler.Parser (AST(..))


data Bean = Byte Word8 
          | Reference String
          deriving (Show, Eq)

data Context = Context { _beans   :: [Bean]
                       , _labels  :: M.Map String Int 
                       } deriving (Show, Eq)
type Emitter = AST -> State Context ()

empty :: Context
empty = Context { _beans = [], _labels = M.fromList [] }

emitLabelDef :: Emitter
emitLabelDef (LabelDef label) = do
  ctx <- get
  let labels = _labels ctx
  let current = length (_beans ctx)
  put ctx { _labels = M.insert label current labels }
  return ()
emitLabelDef _ = return()

emitParam :: Emitter
emitParam (Param (Integer x)) = emitByte $ fromIntegral $ x
emitParam (Param (LabelRef label)) = emitBean $ Reference $ label
emitParam _ = return()

emitInstr :: Emitter
emitInstr (Instruction (Operator op) Empty) = emitByte $ fromIntegral . fromEnum $ op
emitInstr (Instruction (Operator op) (Params params)) = do
  emitByte $ fromIntegral . fromEnum $ op
  mapM_ emitParam params 
  return ()
emitInstr _ = return()

emitLine :: Emitter
emitLine (Line labelDef Empty) = emitLabelDef labelDef
emitLine (Line Empty instr) = emitInstr instr
emitLine (Line labelDef instr) = emitLabelDef labelDef >> emitInstr instr >> return ()
emitLine _ = return()

emitProgram :: Emitter
emitProgram (Program progLines) = mapM emitLine progLines >> return ()
emitProgram _ = return()

emitByte :: Word8 -> State Context ()
emitByte byte = emitBean $ Byte $ byte

emitBean :: Bean -> State Context ()
emitBean bean = do
  ctx <- get
  put ctx { _beans = _beans ctx ++ [bean] }
  return ()

resolveLabels :: M.Map String Int -> [Bean] -> Either String [Bean]
resolveLabels labels beans = sequence $ foldr folder [] beans
  where
    folder b acc = (resolveLabel labels b) : acc

resolveLabel :: M.Map String Int -> Bean -> Either String Bean
resolveLabel _ b@(Byte _) = Right b
resolveLabel labels (Reference label) = case M.lookup label labels of
    (Just t) -> Right . Byte . fromIntegral $ t
    Nothing  -> Left $ "Label '" ++ label ++ "' is not defined"  

emit :: AST -> Either String [Word8]
emit root = do  
  let ctx = execState (emitProgram root) empty
  let labels = _labels ctx
  let beans = _beans ctx
  resolved <- resolveLabels labels beans
  return $ map (\(Byte b) -> b) resolved  
    
  