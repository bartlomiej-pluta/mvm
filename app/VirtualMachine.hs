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

import Data.Word (Word8)
import Data.Foldable (toList)
import Data.Char (chr, toLower, toUpper)

import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT(..), except)

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.ByteString as B

import Util (byteStr, bytesStr)


data VM = VM { _pc :: Int
             , _fp :: Int
             , _stack :: S.Seq Int
             , _halt :: Bool
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
        | In   -- 0x16
        | Out  -- 0x17
        deriving (Eq, Ord, Enum, Show, Read, Bounded)

type Args = [Int]
type Pops = [Int]
type Pushes = S.Seq Int

data Instruction = Simple     { _op        :: Op
                              , _noParams  :: Int
                              , _noPops    :: Int
                              , _sAction   :: Args -> Pops -> Pushes
                              } 
                   | Complex  { _op        :: Op
                              , _noParams  :: Int
                              , _noPops    :: Int
                              , _cAction   :: VM -> Args -> Pops -> ExceptT String IO VM
                              } 

data Command = Command { _instr  :: Instruction
                       , _args   :: [Int]
                       } 

empty :: VM
empty = VM { _pc     = 0
           , _fp     = -1
           , _stack  = S.empty
           , _halt   = False
           }

instructions :: [Instruction]
instructions = [ Simple  { _op = Nop,  _noParams = 0, _noPops = 0, _sAction = (\_ _      -> S.empty)                               }
               , Simple  { _op = Push, _noParams = 1, _noPops = 0, _sAction = (\params _ -> S.fromList params)                     }
               , Simple  { _op = Pop,  _noParams = 0, _noPops = 1, _sAction = (\_ _      -> S.empty)                               }
               , Simple  { _op = Dup,  _noParams = 0, _noPops = 1, _sAction = (\_ [x]    -> S.fromList [x, x])                     }
               , Simple  { _op = Swap, _noParams = 0, _noPops = 2, _sAction = (\_ [x, y] -> S.fromList [y, x])                     }
               , Simple  { _op = Add,  _noParams = 0, _noPops = 2, _sAction = (\_ [x, y] -> S.fromList [y + x])                    }
               , Simple  { _op = Sub,  _noParams = 0, _noPops = 2, _sAction = (\_ [x, y] -> S.fromList [y - x])                    }
               , Simple  { _op = Mul,  _noParams = 0, _noPops = 2, _sAction = (\_ [x, y] -> S.fromList [y * x])                    }
               , Simple  { _op = Div,  _noParams = 0, _noPops = 2, _sAction = (\_ [x, y] -> S.fromList [y `div` x])                }
               , Simple  { _op = Neg,  _noParams = 0, _noPops = 1, _sAction = (\_ [x]    -> S.fromList [-x])                       }
               , Simple  { _op = Not,  _noParams = 0, _noPops = 1, _sAction = (\_ [x]    -> S.fromList [if x /= 0 then 0 else 1])  }
               , Complex { _op = Halt, _noParams = 0, _noPops = 1, _cAction = (\vm _ _   -> except $ Right $ vm { _halt = True })  }
               , Complex { _op = Jmp,  _noParams = 1, _noPops = 1, _cAction = (\vm [x] _ -> except $ Right $ vm { _pc = x})        }
               , Complex { _op = Je,   _noParams = 1, _noPops = 1, _cAction = jumpIf (==)                                          }
               , Complex { _op = Jne,  _noParams = 1, _noPops = 1, _cAction = jumpIf (/=)                                          }
               , Complex { _op = Jg,   _noParams = 1, _noPops = 1, _cAction = jumpIf (>)                                           }
               , Complex { _op = Jl,   _noParams = 1, _noPops = 1, _cAction = jumpIf (<)                                           }
               , Complex { _op = Jge,  _noParams = 1, _noPops = 1, _cAction = jumpIf (>=)                                          }
               , Complex { _op = Jle,  _noParams = 1, _noPops = 1, _cAction = jumpIf (<=)                                          }
               , Complex { _op = Out,  _noParams = 0, _noPops = 1, _cAction = output                                               }
               ]

jumpIf :: (Int -> Int -> Bool) -> VM -> Args -> Pops -> ExceptT String IO VM
jumpIf _ _ [] _  = except $ Left $ "Address expected"
jumpIf _ _ _  [] = except $ Left $ "Empty stack - nothing to compare"
jumpIf predicate vm (addr:_) (top:_) = except $ Right $ vm { _pc = pc }
  where pc = if top `predicate` 0 then addr else _pc vm + 1

output :: VM -> Args -> Pops -> ExceptT String IO VM
output _ _ [] = except $ Left $ "Empty stack - nothing to output"
output vm _ (char:_) = do
  liftIO $ putStr $ [chr char]
  return vm { _pc = _pc vm + 1, _stack = S.drop 1 $ _stack vm}

instructionByOp :: M.Map Op Instruction
instructionByOp = M.fromList $ map (\i -> (_op i, i)) instructions

toOp :: String -> Op
toOp = read . capitalize
  where capitalize :: String -> String
        capitalize [] = []
        capitalize (x:xs) = toUpper x : map toLower xs

parse :: B.ByteString -> Either String [Command]
parse = parseCommands . B.unpack

parseCommands :: [Word8] -> Either String [Command]
parseCommands [] = Right []
parseCommands code@(x:_) = case parseCommand code of
  Just (cmd, rest) -> parseCommands rest >>= (\r -> return $ cmd : r)                              
  Nothing          -> Left $ "Unparseable byte: " ++ byteStr x ++ "\nIn following sequence:\n" ++ bytesStr 16 code

parseCommand :: [Word8] -> Maybe (Command, [Word8])
parseCommand [] = Nothing
parseCommand (opByte:xs) = do
  let op = toEnum . fromIntegral $ opByte :: Op
  instruction <- M.lookup op instructionByOp
  let paramsNumber = _noParams instruction
  let params = map fromIntegral $ take paramsNumber xs :: [Int]
  return (Command instruction params, drop paramsNumber xs)

interpret :: [Command] -> VM -> ExceptT String IO VM
interpret _ vm@(VM _ _ _ True) = except $ Right $ vm
interpret cmds vm = do
  vm' <- interpretCommand cmds vm
  interpret cmds vm'

interpretCommand :: [Command] -> VM -> ExceptT String IO VM
interpretCommand [] _ = except $ Left $ "Empty code"
interpretCommand cmds vm@(VM pc _ _ _)
  | pc >= length cmds = except $ Right $ vm { _halt = True }
  | otherwise         = case instr of
                          (Simple _ _ _ _)  -> except $ interpretSimple vm cmd
                          (Complex _ _ _ _) -> interpretComplex vm cmd
                          where cmd@(Command instr _) = cmds !! pc

interpretSimple :: VM -> Command -> Either String VM
interpretSimple vm (Command (Simple _ _ noPops operation) args) = vm'
  where
    pops = toList . S.take noPops . _stack $ vm
    stack' = Right $ operation args pops
    vm' = stack' >>= (\s -> Right $ vm { _pc = _pc vm + 1
                                       , _stack = s <> (S.drop noPops . _stack) vm
                                       })
interpretSimple _ _ = Left "Unknown operation"

interpretComplex :: VM -> Command -> ExceptT String IO VM
interpretComplex vm (Command (Complex _ _ noPops operation) args) = operation vm args pops
  where
    pops = toList . S.take noPops . _stack $ vm
interpretComplex _ _ = except $ Left "Unknown operation"

run :: VM -> B.ByteString -> ExceptT String IO VM
run vm code = return code >>= (except . parse) >>= flip interpret vm