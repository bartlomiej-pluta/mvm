module VirtualMachine (
  VM(..),
  Op(..),
  Instruction(..),
  Command(..),
  empty,
  instructions,
  instructionByOp,
  toOp,
  run
) where

import Data.Word
import Data.Foldable
import qualified Data.Char as Char
import qualified Data.Map as Map
import qualified Data.Sequence as S
import qualified Data.ByteString as B

import qualified Util as U

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

parse :: B.ByteString -> Either String [Command]
parse = parseCommands . B.unpack

parseCommands :: [Word8] -> Either String [Command]
parseCommands [] = Right []
parseCommands code@(x:_) = case parseCommand code of
  Just (cmd, rest) -> parseCommands rest >>= (\r -> return $ cmd : r)                              
  Nothing          -> Left $ "Unparseable byte: " ++ U.byteStr x ++ "\nIn following sequence:\n" ++ U.bytesStr 16 code

parseCommand :: [Word8] -> Maybe (Command, [Word8])
parseCommand [] = Nothing
parseCommand (opByte:xs) = do
  let op = toEnum . fromIntegral $ opByte :: Op
  instruction <- Map.lookup op instructionByOp
  let paramsNumber = noParams instruction
  let params = map fromIntegral $ take paramsNumber xs :: [Int]
  return (Command instruction params, drop paramsNumber xs)

interpret :: [Command] -> VM -> Either String VM
interpret _ vm@(VM _ _ _ True) = Right $ vm
interpret cmds vm = do
  vm' <- interpretCommand cmds vm
  interpret cmds vm'

interpretCommand :: [Command] -> VM -> Either String VM
interpretCommand [] _ = Left $ "Empty code"
interpretCommand cmds vm@(VM pc _ _ _)
  | pc >= length cmds = Right $ vm { halt = True }
  | otherwise         = case instr of
                          (Simple _ _ _ _) -> interpretSimple vm cmd
                          (Complex _ _ _)    -> interpretComplex vm cmd
                          where cmd@(Command instr _) = cmds !! pc

interpretSimple :: VM -> Command -> Either String VM
interpretSimple vm (Command (Simple op _ noPops operation) args) = vm'
  where
    pops = toList . S.take noPops . stack $ vm
    stack' = Right $ operation args pops
    vm' = stack' >>= (\s -> Right $ vm { pc = pc vm + 1
                                       , stack = s <> (S.drop noPops . stack) vm
                                       })
interpretSimple _ _ = Left "Unknown operation"

interpretComplex :: VM -> Command -> Either String VM
interpretComplex vm (Command (Complex _ _ operation) args) = operation vm args
interpretComplex _ _ = Left "Unknown operation"

run :: VM -> B.ByteString -> Either String VM
run vm code = parse code >>= flip interpret vm