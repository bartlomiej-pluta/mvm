module VirtualMachine.VM where

import Data.Foldable (toList)
import Control.Monad.Trans (lift)
import Control.Monad.State (get, put)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Sequence as S


data VM = VM { _pc    :: Int
             , _fp    :: Int
             , _stack :: S.Seq Int
             , _halt  :: Bool
             , _debug :: Bool
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
        | Lda  -- 0x15
        | In   -- 0x16
        | Out  -- 0x17
        | Clr  -- 0x18
        | Roll -- 0x19
        | Over -- 0x1A
        deriving (Eq, Ord, Enum, Show, Read, Bounded)

type Machine = StateT VM IO

empty :: VM
empty = VM { _pc     = 0
           , _fp     = -1
           , _stack  = S.empty
           , _halt   = False
           , _debug  = False
           }

-------------------------------------------------------------------------------

getPc :: Machine Int
getPc = get >>= (return . _pc)

getFp :: Machine Int
getFp = get >>= (return . _fp)

isHalted :: Machine Bool
isHalted = get >>= (return . _halt)

isDebug :: Machine Bool
isDebug = get >>= (return . _debug)

getAt :: Int -> String -> ExceptT String Machine Int
getAt index err = do
  vm <- lift $ get
  let stack = _stack vm  
  case (stack S.!? index) of
    (Just i) -> return i
    Nothing  -> throwError err

getStackSize :: Machine Int
getStackSize = get >>= (return . length . _stack)

setPc :: Int -> Machine ()
setPc pc = do
  vm <- get
  put vm { _pc = pc }

setFp :: Int -> Machine ()
setFp fp = do
  vm <- get
  put vm { _fp = fp }

setHalt :: Bool -> Machine ()
setHalt halt = do
  vm <- get
  put vm { _halt = halt }

pop :: Int -> Machine [Int]
pop count = do
  vm <- get
  let stack = _stack vm
  put vm { _stack = S.drop count $ stack }
  return $ toList $ S.take count $ stack

push :: [Int] -> Machine ()
push = pushS . S.fromList

pushS :: S.Seq Int -> Machine ()
pushS numbers = do
  vm <- get
  put vm { _stack = numbers <> _stack vm  }
  return ()  

forward :: Int -> Machine ()
forward offset = do
  vm <- get
  put vm { _pc = _pc vm + offset }
  return () 