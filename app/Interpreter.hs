module Interpreter (
  interpret,
) where

import Data.Word
import Control.Monad
import qualified Data.Sequence as S

import qualified Command as C
import qualified Instruction as I
import qualified VirtualMachine as VM

interpret :: [C.Command] -> VM.VM -> Either String VM.VM
interpret _ vm@(VM.VM _ _ _ True) = Right $ vm
interpret cmds vm = do
  vm' <- interpretCommand cmds vm
  interpret cmds vm'

interpretCommand :: [C.Command] -> VM.VM -> Either String VM.VM
interpretCommand [] _ = Left $ "Empty code"
interpretCommand cmds vm@(VM.VM pc _ _ _)
  | pc >= length cmds = Right $ vm { VM.halt = True }
  | otherwise         = case instr of
                          (I.Simple _ _ _) -> interpretSimple vm cmd
                          (I.Complex _)    -> interpretComplex vm cmd
                          where cmd@(C.Command instr _) = cmds !! pc

interpretSimple :: VM.VM -> C.Command -> Either String VM.VM
interpretSimple vm (C.Command (I.Simple op _ noPops) args) = vm'
  where
    pops = [1, 2, 3]
    stack' = case op of
      I.Nop -> Right $ nop vm args pops
      I.Halt -> Right $ nop vm args pops
      I.Push -> Right $ push vm args pops
      _ -> Left $ "Unknown operator"
    vm' = stack' >>= (\s -> Right $ vm { VM.pc = VM.pc vm + 1
                                       , VM.stack = VM.stack vm <> s
                                       })
interpretSimple _ _ = Left $ "Unknown operation"

interpretComplex :: VM.VM -> C.Command -> Either String VM.VM
interpretComplex _ _ = Left "Not implemented yet"

nop :: VM.VM -> [Int] -> [Int] -> S.Seq Int
nop vm _ _ = S.empty

push :: VM.VM -> [Int] -> [Int] -> S.Seq Int
push vm args _ = S.fromList args