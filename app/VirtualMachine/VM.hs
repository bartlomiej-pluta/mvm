module VirtualMachine.VM where

import Text.Printf (printf)
import Data.Foldable (toList)
import Control.Monad.Trans (lift)
import Control.Monad.Except (throwError)
import Control.Monad.Trans.State (StateT)
import Control.Monad.Trans.Except (ExceptT)
import qualified Data.Sequence as S
import qualified Control.Monad.State as ST (get, put)
import Data.Functor ((<&>))
import Control.Monad (unless)
import Util (maybeToExcept)


data VM = VM { _pc    :: Int
             , _fp    :: Int
             , _stack :: S.Seq Int
             , _halt  :: Bool
             , _debug :: Bool
             } deriving (Eq)

instance Show VM where
  show vm = printf "%s PC=%03d FP=%03d Stack=%s" (if _halt vm then "\x25A0" else "\x25B6") (_pc vm) (_fp vm) (show $ toList $ _stack vm)

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
        | Over -- 0x19
        | Ldl  -- 0x1A
        | Stl  -- 0x1B
        deriving (Eq, Ord, Enum, Show, Read, Bounded)

type Machine = StateT VM IO

type Computation = ExceptT String Machine

empty :: VM
empty = VM { _pc     = 0
           , _fp     = -1
           , _stack  = S.empty
           , _halt   = False
           , _debug  = False
           }

-------------------------------------------------------------------------------

get :: Computation VM
get = lift ST.get

put :: VM -> Computation ()
put x = lift $ ST.put x

getPc :: Computation Int
getPc = get <&> _pc

getFp :: Computation Int
getFp = get <&> _fp

isHalted :: Computation Bool
isHalted = get <&> _halt

isDebug :: Computation Bool
isDebug = get <&> _debug

stackAt :: Int -> String -> Computation Int
stackAt index err = get >>= \vm -> maybeToExcept (_stack vm S.!? index) err

frameAt :: Int -> (Int -> Int) -> String -> Computation Int
frameAt index t name = do
  vm <- get
  fp <- getFp
  unless (fp > -1) (throwError "No active stack frame")
  stackSize <- getStackSize
  maybeToExcept (_stack vm S.!? (stackSize - fp - 1 - t index)) $ "Cannot determine " ++ name ++ " - index " ++ show index ++ " out of frame bounds"

updateFrameAt :: Int -> Int -> Computation ()
updateFrameAt index value = do
  vm <- get
  fp <- getFp
  unless (fp > -1) (throwError "No active stack frame")
  stackSize <- getStackSize
  put vm { _stack = S.update (stackSize - fp - 1 - index) value $ _stack vm }

getStackSize :: Computation Int
getStackSize = get <&> (length . _stack)

setPc :: Int -> Computation ()
setPc pc = get >>= \vm -> put vm { _pc = pc }

setFp :: Int -> Computation ()
setFp fp = get >>= \vm -> put vm { _fp = fp }

setHalt :: Bool -> Computation ()
setHalt halt = get >>= \vm -> put vm { _halt = halt }

pop :: Int -> Computation [Int]
pop count = do
  vm <- get
  let stack = _stack vm
  put vm { _stack = S.drop count stack }
  return $ toList $ S.take count stack

push :: [Int] -> Computation ()
push = pushS . S.fromList

pushS :: S.Seq Int -> Computation ()
pushS numbers = get >>= \vm -> put vm { _stack = numbers <> _stack vm }

forward :: Int -> Computation ()
forward offset = get >>= \vm -> put vm { _pc = _pc vm + offset }