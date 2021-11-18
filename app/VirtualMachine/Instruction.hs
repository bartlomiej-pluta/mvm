{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module VirtualMachine.Instruction where

import Data.Char (chr, ord)
import Data.Word (Word8)
import Control.Monad (void)
import Control.Monad.Except (throwError)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as M
import qualified Data.Sequence as S

import VirtualMachine.VM (Op(..), Computation, push, pop, forward, getPc, getFp, getStackSize, setPc, setFp, setHalt, frameAt, updateFrameAt)


type Params = [Int]
type Pops = [Int]
type Pushes = S.Seq Int

data Instruction = Simple  { _op :: Op, _noParams :: Int, _noPops :: Int, _sAction :: Params -> Pops -> Pushes         }
                |  Complex { _op :: Op, _noParams :: Int, _noPops :: Int, _cAction :: Params -> Pops -> Computation () }

instance Show Instruction where
  show (Simple op noParams noPops _)  = show op ++ "(S," ++ show noParams ++ "," ++ show noPops ++ ")"
  show (Complex op noParams noPops _) = show op ++ "(C," ++ show noParams ++ "," ++ show noPops ++ ")"

data Unit = Instr { _instr :: Instruction }
          | Byte  { _byte  :: Word8       }
          deriving (Show)

instructions :: [Instruction]
instructions = [ Simple  { _op = Nop,  _noParams = 0, _noPops = 0, _sAction = \_ _      -> S.empty                                 }
               , Simple  { _op = Push, _noParams = 1, _noPops = 0, _sAction = \params _ -> S.fromList params                       }
               , Simple  { _op = Pop,  _noParams = 0, _noPops = 1, _sAction = \_ _      -> S.empty                                 }
               , Simple  { _op = Dup,  _noParams = 0, _noPops = 1, _sAction = \_ [x]    -> S.fromList [x, x]                       }
               , Simple  { _op = Swap, _noParams = 0, _noPops = 2, _sAction = \_ [x, y] -> S.fromList [y, x]                       }
               , Simple  { _op = Add,  _noParams = 0, _noPops = 2, _sAction = \_ [x, y] -> S.fromList [y + x]                      }
               , Simple  { _op = Sub,  _noParams = 0, _noPops = 2, _sAction = \_ [x, y] -> S.fromList [y - x]                      }
               , Simple  { _op = Mul,  _noParams = 0, _noPops = 2, _sAction = \_ [x, y] -> S.fromList [y * x]                      }
               , Simple  { _op = Div,  _noParams = 0, _noPops = 2, _sAction = \_ [x, y] -> S.fromList [y `div` x]                  }
               , Simple  { _op = Neg,  _noParams = 0, _noPops = 1, _sAction = \_ [x]    -> S.fromList [-x]                         }
               , Simple  { _op = Not,  _noParams = 0, _noPops = 1, _sAction = \_ [x]    -> S.fromList [if x /= 0 then 0 else 1]    }
               , Simple  { _op = Over, _noParams = 0, _noPops = 2, _sAction = \_ [x, y] -> S.fromList [y, x, y]                    }
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
               , Complex { _op = Lda,  _noParams = 1, _noPops = 0, _cAction = loadArg                                              }
               , Complex { _op = In,   _noParams = 0, _noPops = 0, _cAction = input                                                }
               , Complex { _op = Out,  _noParams = 0, _noPops = 1, _cAction = output                                               }
               , Complex { _op = Clr,  _noParams = 1, _noPops = 0, _cAction = clear                                                }
               , Complex { _op = Ldl,  _noParams = 1, _noPops = 0, _cAction = loadLocal                                            }
               , Complex { _op = Stl,  _noParams = 1, _noPops = 1, _cAction = storeLocal                                           }
               ]

instructionByOp :: M.Map Op Instruction
instructionByOp = M.fromList $ map (\i -> (_op i, i)) instructions

halt :: Params -> Pops -> Computation ()
halt _ _ = setHalt True

call :: Params -> Pops -> Computation  ()
call (addr:_) _ = do
  fp  <- getFp
  fp' <- getStackSize
  retAddr <- (+2) <$> getPc

  push [retAddr, fp]
  setPc addr
  setFp fp'

ret :: Params -> Pops -> Computation ()
ret _ _ = do
  fp <- getFp
  stackSize <- getStackSize

  fp'     <- frameAt 0 id "frame pointer (fp)"
  retAddr <- frameAt 1 id "return address"

  if stackSize - fp == 2
  then void $ pop (stackSize - fp)
  else pop 1 >>= \retVal -> pop (stackSize - fp - 1) >> push retVal

  setFp fp'
  setPc retAddr

jump :: Params -> Pops -> Computation ()
jump (addr:_) _ = setPc addr

jumpIf :: (Int -> Int -> Bool) -> Params -> Pops -> Computation ()
jumpIf p (addr:_) (top:_) = push [top] >> getPc >>= (\pc -> return $ if top `p` 0 then addr else pc + 2) >>= setPc

input :: Params -> Pops -> Computation ()
input _ _ = liftIO getChar >>= \c -> push [ord c] >> forward 1

output :: Params -> Pops -> Computation ()
output _ (char:_) = liftIO (putStr [chr char]) >> forward 1

loadArg :: Params -> Pops -> Computation ()
loadArg (index:_) _ = frameAt index (\x -> -x - 1) "call argument" >>= \val -> push [val] >> forward 2

clear :: Params -> Pops -> Computation ()
clear (count:_) _ = pop 1 >>= \top -> pop count >> push top >> forward 2

loadLocal :: Params -> Pops -> Computation ()
loadLocal (index:_) _ = frameAt index (+2) "local variable" >>= \val -> push [val] >> forward 2

storeLocal :: Params -> Pops -> Computation ()
storeLocal (index:_) (val:_) = updateFrameAt (index + 2) val >> forward 2

niy :: Op -> Params -> Pops -> Computation ()
niy op _ _ = getPc >>= \pc -> throwError $ "Instruction '" ++ show op ++ "' ("++ show pc ++") is not implemented yet"