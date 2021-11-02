module Command (
  Command(..)
) where

import qualified Instruction as I

data Command = Command { instr  :: I.Instruction
                       , args   :: [Int]
                       } deriving (Eq, Show)