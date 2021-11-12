module VirtualMachine.Instruction where

import Data.Char (chr, ord)
import Data.Word (Word8)
import System.IO (stdin, hGetChar)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Except (ExceptT, except)
import qualified Data.Map as M
import qualified Data.Sequence as S

import VirtualMachine.VM (Op(..), Machine, push, pop, forward, getAt, getPc, getFp, getStackSize, setPc, setFp, setHalt)


type Params = [Int]
type Pops = [Int]
type Pushes = S.Seq Int

data Instruction = Simple  { _op :: Op, _noParams :: Int, _noPops :: Int, _sAction :: Params -> Pops -> Pushes                    } 
                |  Complex { _op :: Op, _noParams :: Int, _noPops :: Int, _cAction :: Params -> Pops -> ExceptT String Machine () } 

instance Show Instruction where
  show (Simple op noParams noPops _)  = (show op) ++ "(S," ++ (show noParams) ++ "," ++ (show noPops) ++ ")"
  show (Complex op noParams noPops _) = (show op) ++ "(C," ++ (show noParams) ++ "," ++ (show noPops) ++ ")"

data Unit = Instr { _instr :: Instruction }
          | Byte  { _byte  :: Word8       }
          deriving (Show)

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
               , Complex { _op = Halt, _noParams = 0, _noPops = 0, _cAction = halt                                                 }
               , Complex { _op = Call, _noParams = 1, _noPops = 0, _cAction = call                                                 }
               , Complex { _op = Ret,  _noParams = 0, _noPops = 0, _cAction = ret                                                  }
               , Complex { _op = Jmp,  _noParams = 1, _noPops = 0, _cAction = jump                                                 }
               , Complex { _op = Je,   _noParams = 1, _noPops = 1, _cAction = jumpIf (==)                                          }
               , Complex { _op = Jne,  _noParams = 1, _noPops = 1, _cAction = jumpIf (/=)                                          }
               , Complex { _op = Jg,   _noParams = 1, _noPops = 1, _cAction = jumpIf (>)                                           }
               , Complex { _op = Jl,   _noParams = 1, _noPops = 1, _cAction = jumpIf (<)                                           }
               , Complex { _op = Jge,  _noParams = 1, _noPops = 1, _cAction = jumpIf (>=)                                          }
               , Complex { _op = Jle,  _noParams = 1, _noPops = 1, _cAction = jumpIf (<=)                                          }
               , Complex { _op = Ld,   _noParams = 1, _noPops = 0, _cAction = load                                                 }
               , Complex { _op = In,   _noParams = 0, _noPops = 0, _cAction = input                                                }
               , Complex { _op = Out,  _noParams = 0, _noPops = 1, _cAction = output                                               }
               , Complex { _op = Clr,  _noParams = 1, _noPops = 0, _cAction = clear                                                }
               ]

instructionByOp :: M.Map Op Instruction
instructionByOp = M.fromList $ map (\i -> (_op i, i)) instructions    

halt :: Params -> Pops -> ExceptT String Machine ()
halt _ _ = lift $ do
  setHalt True
  return ()

call :: Params -> Pops -> ExceptT String Machine  ()
call (addr:_) _ = lift $ do  
  fp  <- getFp
  fp' <- getStackSize  
  retAddr <- getPc >>= return . (+2)
  
  push [retAddr, fp]
  setPc addr
  setFp fp'  
  
  return ()
call [] _ = throwError "Address excepted"
  
ret :: Params -> Pops -> ExceptT String Machine ()
ret _ _ = do
  fp <- lift getFp
  stackSize <- lift getStackSize

  fp'     <- getAt (stackSize - fp - 1) "Cannot determine previous frame pointer (fp)"
  retAddr <- getAt (stackSize - fp - 2) "Cannot determine return address" 

  if stackSize - fp == 2
  then lift $ do
    _ <-  pop $ stackSize - fp
    return ()
  else lift $ do
    retVal <- pop 1
    _ <- pop $ stackSize - fp - 1
    push retVal
    return ()

  lift $ setFp fp'
  lift $ setPc retAddr

  return ()

jump :: Params -> Pops -> ExceptT String Machine ()
jump (addr:_) _ = lift $ do
  setPc addr
  return ()
jump [] _  = throwError "Address expected"

jumpIf :: (Int -> Int -> Bool) -> Params -> Pops -> ExceptT String Machine ()
jumpIf p (addr:_) (top:_) = lift $ do
  pc <- getPc
  setPc $ if top `p` 0 then addr else pc + 2
  return ()
jumpIf _ [] _  = throwError "Address expected"
jumpIf _ _  [] = throwError "Empty stack - nothing to compare"  

input :: Params -> Pops -> ExceptT String Machine ()
input _ _ = lift $ do
  c <- liftIO $ hGetChar stdin
  push [ord c]
  forward 1
  return()

output :: Params -> Pops -> ExceptT String Machine ()
output _ (char:_) = lift $ do
  liftIO $ putStr $ [chr char]
  forward 1
  return ()
output _ [] = except $ Left $ "Empty stack - nothing to output"

load :: Params -> Pops -> ExceptT String Machine ()
load (index:_) _ = do
  fp <- lift getFp
  stackSize <- lift getStackSize
  val <- getAt (stackSize - fp + index) ("Index " ++ (show index) ++ " out of stack bounds")
  lift $ push [val]
  lift $ forward 2
  return ()
load [] _ = throwError "Local parameter index expected"

niy :: Op -> Params -> Pops -> ExceptT String Machine ()
niy op _ _ = do
  pc <- lift getPc
  throwError $ "Instruction '" ++ (show op) ++ "' ("++ (show $ pc) ++") is not implemented yet"

clear :: Params -> Pops -> ExceptT String Machine ()
clear (count:_) _ = lift $ do
  top <- pop 1
  _ <- pop count
  push top
  forward 2
  return ()
clear [] _ = except $ Left "Number of elements to be cleaned expected"