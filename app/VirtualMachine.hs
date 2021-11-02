module VirtualMachine (
  VM(..),
  empty
)
where

import qualified Data.Sequence as S

data VM = VM { pc :: Int
             , fp :: Int
             , stack :: S.Seq Int
             , halt :: Bool
             } deriving (Show, Eq)

empty :: VM
empty = VM { pc     = 0
           , fp     = -1
           , stack  = S.empty
           , halt   = False
           }