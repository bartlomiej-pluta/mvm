module Assembler.Tokenizer where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.Monoid as Monoid
import qualified VirtualMachine as VM
import qualified Util as U

data Token = Operator VM.Op 
           | IntLiteral Int 
           | StringLiteral String
           | Identifier String
           | Colon
           | Ampersand
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
    mapper = if cs then id else U.toLowerCase
    zipped = zipWith (==) (mapper kwd) (mapper . take len $ input)
    matches = and zipped && len == length zipped

operatorTokenizer :: VM.Op -> Tokenizer
operatorTokenizer op input = keywordTokenizer False (U.toLowerCase . show $ op) (Operator op) input

tokenizeOperators :: Tokenizer
tokenizeOperators = anyTokenizer $ map operatorTokenizer ops
  where
    ops = List.sortBy cmp [VM.Nop ..]
    cmp x y = (length . show) y `compare` (length . show) x

tokenizeIdentifier :: Tokenizer
tokenizeIdentifier [] = Nothing
tokenizeIdentifier input@(x:_) = if null identifier || (not . Char.isAlpha) x
  then Nothing
  else Just $ TokenizeResult (Identifier identifier) (length identifier)
  where
    identifier = takeWhile (or . sequenceA [Char.isAlphaNum, (=='_')]) input

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

tokenizeHex :: Tokenizer
tokenizeHex [] = Nothing
tokenizeHex input = if isPrefix && len > 0
  then Just $ TokenizeResult (IntLiteral number) (len + 2)
  else Nothing
  where
    isPrefix = take 2 input == "0x"
    number = read . ("0x"++) $ numberStr
    len = length numberStr
    numberStr = takeWhile Char.isHexDigit (drop 2 input)

tokenizeChar :: Tokenizer
tokenizeChar ('\'':'\\':x:'\'':_) = U.controlChar x >>= (\s -> return $ TokenizeResult (IntLiteral s) 4)
tokenizeChar ('\'':x:'\'':_) = Just $ TokenizeResult (IntLiteral . Char.ord $ x) 3      
tokenizeChar _ = Nothing

tokenizeString :: Tokenizer
tokenizeString ('"':xs) = do
    string <- extractString xs
    unescaped <- U.unescape string
    return $ TokenizeResult (StringLiteral unescaped) (length string + 2)  
  where
    extractString [] = Nothing
    extractString (y:ys)
        | y == '"'  = Just []
        | y == '\n' = Nothing
        | otherwise = extractString ys >>= (\r -> return $ y : r)
tokenizeString _  = Nothing

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
sepTokenizer _ _ [] = Nothing
sepTokenizer predicate tokenizer input = do
  result@(TokenizeResult _ consumed) <- tokenizer input
  let next = drop consumed input
  let (isSep, _) =  if null next 
                           then (True, 0)
                           else if predicate . head $ next
                             then (True, 1)
                             else (False, 0)
  if isSep
  then return $ result
  else Nothing

anyTokenizer :: [Tokenizer] -> Tokenizer
anyTokenizer _ [] = Nothing
anyTokenizer tokenizers input = Monoid.getFirst . Monoid.mconcat . map Monoid.First $ sequenceA tokenizers input

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize input = tokens >>= (\t -> Right $ filter tokenFilter t)
  where
    tokens = case tokenizers input of
      (Just (TokenizeResult token chars)) -> tokenize (drop chars input) >>= (\rest -> return $ token : rest)
      Nothing                             -> Left $ "Unknown token: " ++ take 20 input
    tokenizers = anyTokenizer
      [ keywordTokenizer False "\n" NewLine
      , tokenizeWhitespace
      , tokenizeComment
      , sepTokenizer Char.isSpace tokenizeOperators
      , sepTokenizer Char.isSpace tokenizeHex
      , sepTokenizer Char.isSpace tokenizeDecimal
      , tokenizeIdentifier
      , keywordTokenizer False ":" Colon
      , keywordTokenizer False "&" Ampersand
      , tokenizeChar
      , tokenizeString
      ]

tokenFilter :: Token -> Bool
tokenFilter (WhiteSpace) = False
tokenFilter (Comment _)  = False
tokenFilter _            = True