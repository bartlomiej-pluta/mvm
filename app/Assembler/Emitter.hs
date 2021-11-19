module Assembler.Emitter where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import Control.Monad.State (State, evalState, get, put)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Word (Word8)

import qualified Data.Map as M

import Assembler.Parser (AST(..), Scope(..))
import Data.Functor ((<&>))
import Util (maybeToExcept, maybeToEither)


data Bean = Byte      { _byte      :: Word8  }
          | Reference { _reference :: String }
          deriving (Show, Eq)

data Context = Context { _beans        :: [Bean]
                       , _labels       :: M.Map String Int
                       , _currentLabel :: Maybe String
                       } deriving (Show, Eq)
type Emitter = AST -> ExceptT String (State Context) ()

empty :: Context
empty = Context { _beans = [], _labels = M.empty, _currentLabel = Nothing }

emitBean :: Bean -> ExceptT String (State Context) ()
emitBean bean = lift $ do
  ctx <- get
  put ctx { _beans = _beans ctx ++ [bean] }

emitByte :: Word8 -> ExceptT String (State Context) ()
emitByte byte = emitBean $ Byte byte

emitParam :: Emitter
emitParam (Param (Integer x))  = emitByte $ fromIntegral x
emitParam (Param (LabelRef Global l)) = emitBean $ Reference l
emitParam (Param (LabelRef Local l)) = do
  ctx <- lift get
  scope <- maybeToExcept (_currentLabel ctx) $ "Local label ('." ++ l ++ "') reference is allowed only in the global label scope"
  emitBean $ Reference (scope ++ "." ++ l)
emitParam _ = throwError "Number or label reference expected"

emitLabelDef :: Emitter
emitLabelDef (LabelDef Global label) = do
  ctx <- lift get
  let labels = _labels ctx
  let current = length (_beans ctx)
  when (label `M.member` labels) (throwError $ "Label '" ++ label ++ "' is already defined")
  put ctx { _labels = M.insert label current labels, _currentLabel = Just label }
emitLabelDef (LabelDef Local label) = do
  ctx <- lift get
  let labels = _labels ctx
  scope <- maybeToExcept (_currentLabel ctx) $ "Local label ('." ++ label ++ "') can be defined only in the global label scope"
  let canonicalLabel = scope ++ "." ++ label
  let current = length (_beans ctx)
  when (canonicalLabel `M.member` labels) (throwError $ "Label '" ++ label ++ "' is already defined in the global label '" ++ scope ++ "' scope")
  put ctx { _labels = M.insert canonicalLabel current labels }
emitLabelDef _ = throwError "Label definition expected"

emitInstr :: Emitter
emitInstr (Instruction (Operator op) Empty) = emitByte $ fromIntegral . fromEnum $ op
emitInstr (Instruction (Operator op) (Params params)) = emitByte (fromIntegral $ fromEnum op) >> mapM_ emitParam params
emitInstr _ = throwError "Instruction expected"

emitLine :: Emitter
emitLine (Line labelDef Empty) = emitLabelDef labelDef
emitLine (Line Empty instr) = emitInstr instr
emitLine (Line labelDef instr) = emitLabelDef labelDef >> emitInstr instr
emitLine _ = throwError "Line of code expected"

emitProgram :: Emitter
emitProgram (Program progLines) = mapM_ emitLine progLines
emitProgram _ = throwError "Program code expected"

resolveLabels :: M.Map String Int -> [Bean] -> Either String [Bean]
resolveLabels labels beans = sequence $ foldr folder [] beans
  where folder b acc = resolveLabel labels b : acc

resolveLabel :: M.Map String Int -> Bean -> Either String Bean
resolveLabel _ b@(Byte _) = Right b
resolveLabel labels (Reference label) = Byte . fromIntegral <$> maybeToEither (M.lookup label labels) ("Label '" ++ label ++ "' is not defined")


emit :: AST -> Either String [Word8]
emit root = evalState (runExceptT $ emitProgram root >> lift get) empty >>= \ctx -> resolveLabels (_labels ctx) (_beans ctx) <&> map _byte

  