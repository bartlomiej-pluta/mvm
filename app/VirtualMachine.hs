module VirtualMachine (
  VM(..),
  Op(..),
  Instruction(..),
  Command(..),
  empty,
  instructions,
  instructionByOp,
  toOp
) where

import Data.Foldable
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Sequence as S

data VM = VM { pc :: Int
             , fp :: Int
             , stack :: S.Seq Int
             , halt :: Bool
             } deriving (Show, Eq)

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
                              , noParams  :: Int
                              , cAction   :: VM -> Args -> Either String VM
                              } 

data Command = Command { instr  :: Instruction
                       , args   :: [Int]
                       } 

empty :: VM
empty = VM { pc     = 0
           , fp     = -1
           , stack  = S.empty
           , halt   = False
           }

instructions :: [Instruction]
instructions = [ Simple  { op = Nop,  noParams = 0, noPops = 0, sAction = (\_ _      -> S.empty)                               }
               , Simple  { op = Push, noParams = 1, noPops = 0, sAction = (\args _   -> S.fromList args)                       }
               , Simple  { op = Pop,  noParams = 0, noPops = 1, sAction = (\_ _      -> S.empty)                               }
               , Simple  { op = Dup,  noParams = 0, noPops = 1, sAction = (\_ [x]    -> S.fromList [x, x])                     }
               , Simple  { op = Swap, noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y, x])                     }
               , Simple  { op = Add,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y + x])                    }
               , Simple  { op = Sub,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y - x])                    }
               , Simple  { op = Mul,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y * x])                    }
               , Simple  { op = Div,  noParams = 0, noPops = 2, sAction = (\_ [x, y] -> S.fromList [y `div` x])                }
               , Simple  { op = Neg,  noParams = 0, noPops = 1, sAction = (\_ [x]    -> S.fromList [-x])                       }
               , Simple  { op = Not,  noParams = 0, noPops = 1, sAction = (\_ [x]    -> S.fromList [if x /= 0 then 0 else 1])  }
               , Complex { op = Halt, noParams = 0, cAction = (\vm _   -> Right $ vm { halt = True })                          }
               , Complex { op = Jmp,  noParams = 1, cAction = (\vm [x] -> Right $ vm { pc = x})                                }
               , Complex { op = Je,   noParams = 1, cAction = jumpIf (==)                                                      }
               , Complex { op = Jne,  noParams = 1, cAction = jumpIf (/=)                                                      }
               , Complex { op = Jg,   noParams = 1, cAction = jumpIf (>)                                                       }
               , Complex { op = Jl,   noParams = 1, cAction = jumpIf (<)                                                       }
               , Complex { op = Jge,  noParams = 1, cAction = jumpIf (>=)                                                      }
               , Complex { op = Jle,  noParams = 1, cAction = jumpIf (<=)                                                      }
               ]

jumpIf :: (Int -> Int -> Bool) -> VM -> Args -> Either String VM
jumpIf predicate vm [addr] = Right $ vm { pc = pc' }
  where
    (top:_) = toList . stack $ vm
    pc' = if top `predicate` 0 then addr else pc vm + 1

instructionByOp :: Map.Map Op Instruction
instructionByOp = Map.fromList $ map (\i -> (op i, i)) instructions

toOp :: String -> Op
toOp = read . capitalize
  where capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = Char.toUpper x : map Char.toLower xs