module Instruction (
  Op(..),
  Instruction(..),
  Command(..),
  instructions,
  instructionByOp,
  toOp
) where

import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Sequence as S

import qualified VirtualMachine as VM

data Op = Nop  -- 0x00 
        | Halt -- 0x01
        | Push -- 0x02
        | Pop  -- 0x03
        | Dup  -- 0x04
        | Swap -- 0x05
        | Add  -- 0x06
        | Sub  -- 0x07
        | Mul  -- 0x08
        | Div  -- 0x09
        | Neg  -- 0x0a
        | Not  -- 0x0b
        | Call -- 0x0c
        | Ret  -- 0x0d
        | Jmp  -- 0x0e
        | Je   -- 0x0f
        | Jne  -- 0x10
        | Jg   -- 0x11
        | Jl   -- 0x12
        | Jge  -- 0x13
        | Jle  -- 0x14
        | Ld   -- 0x15
        deriving (Eq, Ord, Enum, Show, Read, Bounded)

type Args = [Int]
type Pops = [Int]
type Pushes = S.Seq Int

data Instruction = Simple     { op        :: Op
                              , noParams  :: Int
                              , noPops    :: Int
                              , sAction   :: Args -> Pops -> Pushes
                              } 
                   | Complex  { op        :: Op
                              , cAction   :: VM.VM -> Command -> Either String VM.VM
                              } 

data Command = Command { instr  :: Instruction
                       , args   :: [Int]
                       } 

instructions :: [Instruction]
instructions = [ Simple { op = Nop,  noParams = 0, noPops = 0, sAction = (\_ _      -> S.empty)                               }
               , Simple { op = Push, noParams = 1, noPops = 0, sAction = (\args _   -> S.fromList args)                       }
               , Simple { op = Pop,  noParams = 0, noPops = 1, sAction = (\_ _      -> S.empty)                               }
               , Simple { op = Dup,  noParams = 0, noPops = 1, sAction = (\_ [x]    -> S.fromList [x, x])                     }
               , Simple { op = Swap, noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y, x])                     }
               , Simple { op = Add,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y + x])                    }
               , Simple { op = Sub,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y - x])                    }
               , Simple { op = Mul,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y * x])                    }
               , Simple { op = Div,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y `div` x])                }
               , Simple { op = Neg,  noParams = 0, noPops = 1, sAction = (\_ [x]    -> S.fromList [-x])                       }
               , Simple { op = Not,  noParams = 0, noPops = 1, sAction = (\_ [x]    -> S.fromList [if x /= 0 then 0 else 1])  }
               ]

instructionByOp :: Map.Map Op Instruction
instructionByOp = Map.fromList $ map (\i -> (op i, i)) instructions

toOp :: String -> Op
toOp = read . capitalize
  where capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = Char.toUpper x : map Char.toLower xs