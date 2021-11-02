module Interpreter (
  interpret,
) where

import Data.Word
import Control.Monad
import Data.Foldable
import qualified Data.Sequence as S

import qualified VirtualMachine as VM

interpret :: [VM.Command] -> VM.VM -> Either String VM.VM
interpret _ vm@(VM.VM _ _ _ True) = Right $ vm
interpret cmds vm = do
  vm' <- interpretCommand cmds vm
  interpret cmds vm'

interpretCommand :: [VM.Command] -> VM.VM -> Either String VM.VM
interpretCommand [] _ = Left $ "Empty code"
interpretCommand cmds vm@(VM.VM pc _ _ _)
  | pc >= length cmds = Right $ vm { VM.halt = True }
  | otherwise         = case instr of
                          (VM.Simple _ _ _ _) -> interpretSimple vm cmd
                          (VM.Complex _ _ _)    -> interpretComplex vm cmd
                          where cmd@(VM.Command instr _) = cmds !! pc

interpretSimple :: VM.VM -> VM.Command -> Either String VM.VM
interpretSimple vm (VM.Command (VM.Simple op _ noPops operation) args) = vm'
  where
    pops = toList . S.take noPops . VM.stack $ vm
    stack' = Right $ operation args pops
    vm' = stack' >>= (\s -> Right $ vm { VM.pc = VM.pc vm + 1
                                       , VM.stack = s <> (S.drop noPops . VM.stack) vm
                                       })
interpretSimple _ _ = Left "Unknown operation"

interpretComplex :: VM.VM -> VM.Command -> Either String VM.VM
interpretComplex vm (VM.Command (VM.Complex _ _ operation) args) = operation vm args
interpretComplex _ _ = Left "Unknown operation"