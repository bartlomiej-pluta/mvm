module Interpreter (
  interpret,
) where

import Data.Word
import Control.Monad
import Data.Foldable
import qualified Data.Sequence as S

import qualified Instruction as I
import qualified VirtualMachine as VM

interpret :: [I.Command] -> VM.VM -> Either String VM.VM
interpret _ vm@(VM.VM _ _ _ True) = Right $ vm
interpret cmds vm = do
  vm' <- interpretCommand cmds vm
  interpret cmds vm'

interpretCommand :: [I.Command] -> VM.VM -> Either String VM.VM
interpretCommand [] _ = Left $ "Empty code"
interpretCommand cmds vm@(VM.VM pc _ _ _)
  | pc >= length cmds = Right $ vm { VM.halt = True }
  | otherwise         = case instr of
                          (I.Simple _ _ _ _) -> interpretSimple vm cmd
                          (I.Complex _ _)    -> interpretComplex vm cmd
                          where cmd@(I.Command instr _) = cmds !! pc

interpretSimple :: VM.VM -> I.Command -> Either String VM.VM
interpretSimple vm (I.Command (I.Simple op _ noPops operation) args) = vm'
  where
    pops = toList . S.take noPops . VM.stack $ vm
    stack' = Right $ operation args pops
    vm' = stack' >>= (\s -> Right $ vm { VM.pc = VM.pc vm + 1
                                       , VM.stack = s <> (S.drop noPops . VM.stack) vm
                                       })
interpretSimple _ _ = Left $ "Unknown operation"

interpretComplex :: VM.VM -> I.Command -> Either String VM.VM
interpretComplex _ _ = Left "Not implemented yet"