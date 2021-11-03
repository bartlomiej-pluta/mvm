module Assembler (
  tokenize
) where

import Data.Char as Char
import Data.Monoid as Monoid
import qualified VirtualMachine as VM (Op(..), Instruction, Command, instructionByOp)
import qualified Util as U

data Token = Operator VM.Op 
           | KeywordLiteral String 
           | IntLiteral Int 
           | WhiteSpace
           | Comment String
           deriving (Eq, Show)

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

tokenizeOperators :: Tokenizer
tokenizeOperators = anyTokenizer $ map tokenizeOperator [VM.Push ..]

tokenizeWhitespace :: Tokenizer
tokenizeWhitespace [] = Nothing
tokenizeWhitespace (x:_)
  | Char.isSpace x = Just $ TokenizeResult WhiteSpace 1
  | otherwise      = Nothing

tokenizeDecimal :: Tokenizer
tokenizeDecimal [] = Nothing
tokenizeDecimal input = if null numberStr
  then Nothing
  else Just $ TokenizeResult (IntLiteral number) len
  where    
    number = read numberStr
    len = length numberStr
    numberStr = takeWhile Char.isDigit input

tokenizeComment :: Tokenizer
tokenizeComment [] = Nothing
tokenizeComment (x:xs) = if x == ';'
  then Just $ TokenizeResult (Comment comment) (len + 1)
  else Nothing
  where
    len = length comment
    comment = takeWhile (/='\n') xs

type SeparatorPredicate = Char -> Bool
sepTokenizer :: SeparatorPredicate -> Tokenizer -> Tokenizer
sepTokenizer pred tokenizer input = do
  (TokenizeResult token consumed) <- tokenizer input
  let next = drop consumed input
  let (isSep, consumed') = if null next 
                           then (True, 0)
                           else if pred . head $ next
                             then (True, 1)
                             else (False, 0)
  if isSep
  then return $ TokenizeResult token (consumed + consumed')
  else Nothing

anyTokenizer :: [Tokenizer] -> Tokenizer
anyTokenizer tokenizers input = Monoid.getFirst . Monoid.mconcat . map Monoid.First $ sequenceA tokenizers input

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize input = case tokenizers input of
    (Just (TokenizeResult token chars)) -> tokenize (drop chars input) >>= (\rest -> return $ token : rest)
    Nothing                             -> Left $ "Unknown token: " ++ take 20 input

tokenizers :: Tokenizer
tokenizers = anyTokenizer
  [ tokenizeWhitespace
  , tokenizeComment
  , sepTokenizer Char.isSpace tokenizeOperators
  , sepTokenizer Char.isSpace tokenizeDecimal
  ]