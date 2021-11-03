module Assembler (
  tokenize
) where

import Data.Monoid as Monoid
import qualified VirtualMachine as VM (Op(..), Instruction, Command, instructionByOp)
import qualified Util as U

data Token = Operator VM.Op | KeywordLiteral String | IntLiteral Int deriving (Eq, Show)

type ConsumedChars = Int
data TokenizeResult = TokenizeResult Token ConsumedChars deriving (Eq, Show)

type Tokenizer = String -> Maybe TokenizeResult

type CaseSensitive = Bool
tokenizeKeyword :: CaseSensitive -> String -> Tokenizer
tokenizeKeyword _ _ [] = Nothing
tokenizeKeyword cs kwd input
  | matches    = Just $ TokenizeResult (KeywordLiteral . take len $ input) len
  | otherwise  = Nothing
  where
    len = length kwd
    mapper = if cs then id else U.toLowerCase
    zipped = zipWith (==) (mapper kwd) (mapper . take len $ input)
    matches = and zipped && len == length zipped

tokenizeOperator :: VM.Op -> Tokenizer
tokenizeOperator op input = case keywordToken of
  (Just (TokenizeResult _ consumed)) -> Just $ TokenizeResult (Operator op) consumed
  Nothing                            -> Nothing
  where keywordToken = tokenizeKeyword False (U.toLowerCase . show $ op) input

anyTokenizer :: [Tokenizer] -> Tokenizer
anyTokenizer tokenizers input = Monoid.getFirst . Monoid.mconcat . map Monoid.First $ sequenceA tokenizers input

tokenizeOperators :: Tokenizer
tokenizeOperators = anyTokenizer $ map tokenizeOperator [VM.Push ..]

tokenize :: Tokenizer
tokenize = tokenizeOperators
