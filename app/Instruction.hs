module Instruction (
  Op(..),
  Instruction(..),
  instructions,
  instructionByOp,
  toOp
) where

import qualified Data.Char as Char
import qualified Data.Map as Map

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

data Instruction = Simple     { op        :: Op
                              , noParams  :: Int
                              , noPops    :: Int } 
                   | Complex  { op        :: Op
                              }
                   deriving (Eq, Show)

instructions :: [Instruction]
instructions = [ Simple { op = Nop,  noParams = 0, noPops = 0 }
               , Simple { op = Halt, noParams = 0, noPops = 0 }
               , Simple { op = Push, noParams = 1, noPops = 0 }
               , Simple { op = Pop,  noParams = 0, noPops = 1 }
               ]

instructionByOp :: Map.Map Op Instruction
instructionByOp = Map.fromList $ map (\i -> (op i, i)) instructions


toOp :: String -> Op
toOp = read . capitalize
  where capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = Char.toUpper x : map Char.toLower xs