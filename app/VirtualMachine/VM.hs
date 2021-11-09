module VirtualMachine.VM where

import Data.Foldable (toList)
import Control.Monad.State (State, get, put)
import Control.Monad.Trans.Except (ExceptT, except)
import qualified Data.Sequence as S


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
        | Dbg  -- 0x18
        deriving (Eq, Ord, Enum, Show, Read, Bounded)

empty :: VM
empty = VM { _pc     = 0
           , _fp     = -1
           , _stack  = S.empty
           , _halt   = False
           }

-------------------------------------------------------------------------------

push :: [Int] -> State VM ()
push = pushS . S.fromList

pushS :: S.Seq Int -> State VM ()
pushS numbers = do
  vm <- get
  put vm { _stack = numbers <> _stack vm  }
  return ()

pop :: Int -> State VM [Int]
pop count = do
  vm <- get
  let stack = _stack vm
  put vm { _stack = S.drop count $ stack }
  return $ toList $ S.take count $ stack

getAt :: Int -> String -> ExceptT String (State VM) Int
getAt index err = do
  vm <- get
  let stack = _stack vm
  case (stack S.!? index) of
    (Just i) -> return i
    Nothing  -> except $ Left err

getPc :: State VM Int
getPc = get >>= (return . _pc)

getFp :: State VM Int
getFp = get >>= (return . _fp)

getStackSize :: State VM Int
getStackSize = get >>= (return . length . _stack)

setPc :: Int -> State VM ()
setPc pc' = do
  vm <- get
  put vm { _pc = pc' }

setFp :: Int -> State VM ()
setFp fp' = do
  vm <- get
  put vm { _fp = fp' }

forward :: Int -> State VM () 
forward offset = do
  vm <- get
  put vm { _pc = _pc vm + offset }
  return ()  