module VirtualMachine where

import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.ByteString as B

import Data.Char (chr)
import Data.Word (Word8)
import Data.Foldable (toList)
import Control.Monad.State (State, put, get, execState, evalState)
import Control.Monad.Trans (liftIO)
import Control.Monad.Trans.Except (ExceptT, except, runExceptT)

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

call :: VM -> Params -> Pops -> ExceptT String IO VM
call vm (addr:_) _ = except $ return $ vm { _pc = addr, _fp = fp', _stack = stack' }
  where
    fp = _fp vm
    stack = _stack vm
    fp' = length stack
    retAddr = _pc vm + 2
    stack' = S.fromList [retAddr, fp] <> stack
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
  where pc = if top `predicate` 0 then addr else _pc vm + 1
jumpIf _ _ [] _  = except $ Left "Address expected"
jumpIf _ _ _  [] = except $ Left "Empty stack - nothing to compare"  

output :: VM -> Params -> Pops -> ExceptT String IO VM
output vm _ (char:_) = do
  liftIO $ putStr $ [chr char]
  return vm { _pc = _pc vm + 1, _stack = S.drop 1 $ _stack vm}
output _ _ [] = except $ Left $ "Empty stack - nothing to output"

--------------------------------------------------------------------------

push :: [Int] -> State VM ()
push numbers = do
  vm <- get
  put vm { _stack = S.fromList numbers <> _stack vm  }
  return ()

pop :: Int -> State VM [Int]
pop count = do
  vm <- get
  let stack = _stack vm
  put vm { _stack = S.drop count $ stack }
  return $ toList $ S.take count $ stack

popThenPush :: Int -> [Int] -> State VM [Int]
popThenPush count numbers = do
  pops <- pop count
  push numbers
  return pops

getAt :: Int -> String -> ExceptT String (State VM) Int
getAt index err = do
  vm <- get
  let stack = _stack vm
  case (stack S.!? index) of
    (Just i) -> return i
    Nothing  -> except $ Left err

--------------------------------------------------------------------------

instructionByOp :: M.Map Op Instruction
instructionByOp = M.fromList $ map (\i -> (_op i, i)) instructions               

parseInstr :: [Word8] -> Either String (Instruction, [Word8])
parseInstr (opCode:rest) = do
  let op = toEnum . fromIntegral $ opCode :: Op
  instr <- case M.lookup op instructionByOp of
    (Just i) -> Right i
    Nothing -> Left "Unknown instruction"
  let noParams = _noParams instr
  let params = map fromIntegral $ take noParams rest :: [Word8]
  if length params == noParams 
  then return (instr, params)
  else Left $ "Expected " ++ (show noParams) ++ " parameter(s), got " ++ (show $ length params) ++ " for operator '" ++ (show op) ++ "'"
parseInstr [] = Left "Unexpected end of the file"


parse :: [Word8] -> Either String [Unit]
parse [] = Right []
parse code = do
  (instr, params) <- parseInstr code
  let paramBytes = map Byte params
  let noParams = _noParams instr
  rest <- parse (drop (noParams + 1) code)
  return $ [Instr instr] ++ paramBytes ++ rest

interpret :: VM -> [Unit] -> ExceptT String IO VM
interpret vm@VM { _halt = True} _ = except $ Right $ vm
interpret vm units = do
  vm' <- interpretUnit vm units
  interpret vm' units

interpretUnit :: VM -> [Unit] -> ExceptT String IO VM
interpretUnit _ [] = except $ Left "Nothing to interpret"
interpretUnit vm units
  | pc >= progSize = except $ Left $ "PC (=" ++ (show pc) ++ ") exceeds program size (=" ++ (show progSize) ++ ")"
  | otherwise = case unit of
      (Instr instr) -> dispatchInstr vm units instr
      (Byte _)      -> except $ Left $ "PC (=" ++ (show pc) ++ ") currently points to the data byte rather than instruction" 
  where
    pc = _pc vm
    progSize = length units
    unit = units !! pc

dispatchInstr :: VM -> [Unit] -> Instruction -> ExceptT String IO VM
dispatchInstr vm units instr = case instr of 
  Simple {} -> except $ Right $ interpretSimple vm units instr 
  Complex {} -> interpretComplex vm units instr 

interpretSimple :: VM -> [Unit] -> Instruction -> VM
interpretSimple vm units instr = vm'
  where  
    stack = _stack vm
    pc = _pc vm
    noParams = _noParams instr  
    noPops = _noPops instr  

    paramBytes = take noParams $ drop (pc + 1) $ units     :: [Unit]
    params = map (fromIntegral . _byte) paramBytes         :: [Int]
    pops = toList $ S.take noPops $ stack                  :: [Int]

    action = _sAction instr
    pushes = action params pops
    vm' = vm { _pc = pc + noParams + 1, _stack = pushes <> (S.drop noPops stack) }

interpretComplex :: VM -> [Unit] -> Instruction -> ExceptT String IO VM
interpretComplex vm units instr = action vm params pops
  where
    stack = _stack vm
    pc = _pc vm
    noParams = _noParams instr  
    noPops = _noPops instr  
 
    paramBytes = take noParams $ drop (pc + 1) $ units     :: [Unit]
    params = map (fromIntegral . _byte) paramBytes  :: [Int]
    pops = toList $ S.take noPops $ stack           :: [Int]

    action = _cAction instr
    
run :: B.ByteString -> ExceptT String IO VM
run code = (return $ B.unpack code) >>= (except . parse) >>= interpret empty