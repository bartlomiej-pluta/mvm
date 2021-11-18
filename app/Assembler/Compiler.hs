module Assembler.Compiler where

import Data.Word (Word8)

import Assembler.Tokenizer (tokenize)
import Assembler.Parser (parse)
import Assembler.Emitter (emit)

compile :: String -> Either String [Word8]
compile input = tokenize input >>= parse >>= emit
