module Assembler.Tokenizer where

import Data.List (sortBy)
import Data.Char (ord, isDigit, isSpace, isAlpha, isAlphaNum, isHexDigit)
import Data.Monoid (First(..))

import VirtualMachine.VM (Op(..))
import Util (toLowerCase, controlChar, unescape)


data Token = Operator Op
           | IntLiteral Int
           | StringLiteral String
           | Identifier String
           | Colon
           | Ampersand
           | Dot
           | NewLine
           | WhiteSpace
           | Comment String
           deriving (Eq, Show)

type ConsumedChars = Int
data TokenizeResult = TokenizeResult Token ConsumedChars deriving (Eq, Show)

type Tokenizer = String -> Maybe TokenizeResult

type CaseSensitive = Bool
keywordTokenizer :: CaseSensitive -> String -> Token -> Tokenizer
keywordTokenizer _ _ _ [] = Nothing
keywordTokenizer cs kwd token input
  | matches    = Just $ TokenizeResult token len
  | otherwise  = Nothing
  where
    len = length kwd
    mapper = if cs then id else toLowerCase
    zipped = zipWith (==) (mapper kwd) (mapper . take len $ input)
    matches = and zipped && len == length zipped

operatorTokenizer :: Op -> Tokenizer
operatorTokenizer op = keywordTokenizer False (toLowerCase . show $ op) (Operator op)

tokenizeOperators :: Tokenizer
tokenizeOperators = anyTokenizer $ map operatorTokenizer $ sortBy cmp [Nop ..]
  where cmp x y = (length . show) y `compare` (length . show) x

tokenizeIdentifier :: Tokenizer
tokenizeIdentifier [] = Nothing
tokenizeIdentifier input@(x:_) = if null identifier || (not . isAlpha) x
  then Nothing
  else Just $ TokenizeResult (Identifier identifier) (length identifier)
  where identifier = takeWhile (or . sequenceA [isAlphaNum, (=='_')]) input

tokenizeWhitespace :: Tokenizer
tokenizeWhitespace [] = Nothing
tokenizeWhitespace (x:_)
  | isSpace x = Just $ TokenizeResult WhiteSpace 1
  | otherwise = Nothing

tokenizeDecimal :: Tokenizer
tokenizeDecimal [] = Nothing
tokenizeDecimal input = if null numberStr
  then Nothing
  else Just $ TokenizeResult (IntLiteral number) len
  where
    number = read numberStr
    len = length numberStr
    numberStr = takeWhile isDigit input

tokenizeHex :: Tokenizer
tokenizeHex ('0':'x':input) = if null input
  then Nothing
  else Just $ TokenizeResult (IntLiteral $ read $ "0x" ++ numberStr) (length numberStr + 2)
  where numberStr = takeWhile isHexDigit input
tokenizeHex _ = Nothing

tokenizeChar :: Tokenizer
tokenizeChar ('\'':'\\':x:'\'':_) = controlChar x >>= (\s -> return $ TokenizeResult (IntLiteral s) 4)
tokenizeChar ('\'':x:'\'':_) = Just $ TokenizeResult (IntLiteral . ord $ x) 3
tokenizeChar _ = Nothing

tokenizeString :: Tokenizer
tokenizeString ('"':xs) = do
    string <- extractString xs
    unescaped <- unescape string
    return $ TokenizeResult (StringLiteral unescaped) (length string + 2)
  where
    extractString [] = Nothing
    extractString (y:ys)
        | y == '"'  = Just []
        | y == '\n' = Nothing
        | otherwise = extractString ys >>= (\r -> return $ y : r)
tokenizeString _  = Nothing

tokenizeComment :: Tokenizer
tokenizeComment (';':xs) = Just $ TokenizeResult (Comment comment) (length comment + 1)
  where comment = takeWhile (/='\n') xs
tokenizeComment _ = Nothing

type SeparatorPredicate = Char -> Bool
sepTokenizer :: SeparatorPredicate -> Tokenizer -> Tokenizer
sepTokenizer _ _ [] = Nothing
sepTokenizer predicate tokenizer input = do
  result@(TokenizeResult _ consumed) <- tokenizer input
  let next = drop consumed input
  if null next || (predicate . head $ next)
  then return result
  else Nothing

anyTokenizer :: [Tokenizer] -> Tokenizer
anyTokenizer _ [] = Nothing
anyTokenizer tokenizers input = getFirst . mconcat . map First $ sequenceA tokenizers input

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize input = tokens >>= (Right . filter tokenFilter)
  where
    tokens = case tokenizers input of
      (Just (TokenizeResult token chars)) -> tokenize (drop chars input) >>= (\rest -> return $ token : rest)
      Nothing                             -> Left $ "Unknown token: " ++ take 20 input
    tokenizers = anyTokenizer
      [ keywordTokenizer False "\n" NewLine
      , tokenizeWhitespace
      , tokenizeComment
      , sepTokenizer isSpace tokenizeOperators
      , sepTokenizer isSpace tokenizeHex
      , sepTokenizer isSpace tokenizeDecimal
      , tokenizeIdentifier
      , keywordTokenizer False ":" Colon
      , keywordTokenizer False "&" Ampersand
      , keywordTokenizer False "." Dot
      , tokenizeChar
      , tokenizeString
      ]

tokenFilter :: Token -> Bool
tokenFilter WhiteSpace = False
tokenFilter (Comment _)  = False
tokenFilter _            = True