module VirtualMachine.Instruction where

import Data.Char (chr)
import Data.Word (Word8)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)
import Control.Monad.State (execState, evalState)
import qualified Data.Map as M
import qualified Data.Sequence as S

import VirtualMachine.VM (VM(..), Op(..), push, pop, forward, getAt, getPc, getFp, getStackSize, setPc, setFp)


type Params = [Int]
type Pops = [Int]
type Pushes = S.Seq Int

data Instruction = Simple  { _op :: Op, _noParams :: Int, _noPops :: Int, _sAction ::       Params -> Pops -> Pushes               } 
                |  Complex { _op :: Op, _noParams :: Int, _noPops :: Int, _cAction :: VM -> Params -> Pops -> ExceptT String IO VM } 

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
               , Complex { _op = Halt, _noParams = 0, _noPops = 1, _cAction = (\vm _ _   -> except $ Right $ vm { _halt = True })  }
               , Complex { _op = Call, _noParams = 1, _noPops = 0, _cAction = call                                                 }
               , Complex { _op = Ret,  _noParams = 0, _noPops = 0, _cAction = ret                                                  }
               , Complex { _op = Jmp,  _noParams = 1, _noPops = 0, _cAction = (\vm [x] _ -> except $ Right $ vm { _pc = x})        }
               , Complex { _op = Je,   _noParams = 1, _noPops = 1, _cAction = jumpIf (==)                                          }
               , Complex { _op = Jne,  _noParams = 1, _noPops = 1, _cAction = jumpIf (/=)                                          }
               , Complex { _op = Jg,   _noParams = 1, _noPops = 1, _cAction = jumpIf (>)                                           }
               , Complex { _op = Jl,   _noParams = 1, _noPops = 1, _cAction = jumpIf (<)                                           }
               , Complex { _op = Jge,  _noParams = 1, _noPops = 1, _cAction = jumpIf (>=)                                          }
               , Complex { _op = Jle,  _noParams = 1, _noPops = 1, _cAction = jumpIf (<=)                                          }
               , Complex { _op = Out,  _noParams = 0, _noPops = 1, _cAction = output                                               }
               , Complex { _op = Dbg,  _noParams = 0, _noPops = 0, _cAction = debug                                                }
               ]

instructionByOp :: M.Map Op Instruction
instructionByOp = M.fromList $ map (\i -> (_op i, i)) instructions    

call :: VM -> Params -> Pops -> ExceptT String IO VM
call vm (addr:_) _ = except $ return $ flip execState vm $ do  
  fp  <- getFp
  fp' <- getStackSize  
  retAddr  <- getPc >>= return . (+2)
  
  push [retAddr, fp]
  setPc addr
  setFp fp'  
  
  return ()

call _ [] _ = except $ Left $ "Address excepted"

ret :: VM -> Params -> Pops -> ExceptT String IO VM
ret vm _ _ = do
  let fp = _fp vm
  let stack = _stack vm
  let stackSize = S.length stack
  let stack' = _stack $ execState (pop (stackSize - fp)) vm

  fp'     <- except $ evalState (runExceptT (getAt (stackSize - fp - 1) "Cannot determine previous frame pointer (fp)")) vm
  retAddr <- except $ evalState (runExceptT (getAt (stackSize - fp - 2) "Cannot determine return address"             )) vm
  
  return vm { _fp = fp', _pc = retAddr, _stack = stack' }     

debug :: VM -> Params -> Pops -> ExceptT String IO VM
debug vm _ _ = do
  liftIO $ print vm
  return vm { _pc = _pc vm + 1 }

jumpIf :: (Int -> Int -> Bool) -> VM -> Params -> Pops -> ExceptT String IO VM
jumpIf predicate vm (addr:_) (top:_) = except $ Right $ vm { _pc = pc }
  where pc = if top `predicate` 0 then addr else _pc vm + 2
jumpIf _ _ [] _  = except $ Left "Address expected"
jumpIf _ _ _  [] = except $ Left "Empty stack - nothing to compare"  

output :: VM -> Params -> Pops -> ExceptT String IO VM
output vm _ (char:_) = do
  liftIO $ putStr $ [chr char]
  return (execState (forward 1) vm)  
output _ _ [] = except $ Left $ "Empty stack - nothing to output"
