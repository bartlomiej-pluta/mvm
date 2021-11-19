module Assembler.Tokenizer where

import Data.List (sortBy)
import Data.Char (ord, isDigit, isSpace, isAlpha, isAlphaNum, isHexDigit)
import Data.Monoid (First(..))

import VirtualMachine.VM (Op(..))
import Util (toLowerCase, controlChar, unescape, maybeToEither)
import Control.Monad (guard)


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
keywordTokenizer cs kwd token input = guard matches >> return (TokenizeResult token len)
  where len     = length kwd
        matches = mapper kwd == mapper (take len input)
        mapper  = if cs then id else toLowerCase

operatorTokenizer :: Op -> Tokenizer
operatorTokenizer op = keywordTokenizer False (toLowerCase . show $ op) (Operator op)

tokenizeOperators :: Tokenizer
tokenizeOperators = anyTokenizer $ map operatorTokenizer $ sortBy cmp [Nop ..]
  where cmp x y = (length . show) y `compare` (length . show) x

tokenizeIdentifier :: Tokenizer
tokenizeIdentifier [] = Nothing
tokenizeIdentifier input@(x:_) = guard (not $ null identifier) >> guard (isAlpha x) >> return token
  where identifier = takeWhile (or . sequenceA [isAlphaNum, (=='_')]) input
        token      = TokenizeResult (Identifier identifier) (length identifier)

tokenizeWhitespace :: Tokenizer
tokenizeWhitespace [] = Nothing
tokenizeWhitespace (x:_) = guard (isSpace x) >> return (TokenizeResult WhiteSpace 1)

tokenizeDecimal :: Tokenizer
tokenizeDecimal [] = Nothing
tokenizeDecimal input = guard (not $ null numberStr) >> return token
  where numberStr = takeWhile isDigit input
        token     = TokenizeResult (IntLiteral $ read numberStr) $ length numberStr

tokenizeHex :: Tokenizer
tokenizeHex ('0':'x':input) = guard (not $ null numberStr) >> return token
  where numberStr = takeWhile isHexDigit input
        token     = TokenizeResult (IntLiteral $ read $ "0x" ++ numberStr) (length numberStr + 2)
tokenizeHex _ = Nothing

tokenizeChar :: Tokenizer
tokenizeChar ('\'':'\\':x:'\'':_) = controlChar x >>= \s -> return $ TokenizeResult (IntLiteral s) 4
tokenizeChar ('\'':x:'\'':_) = return $ TokenizeResult (IntLiteral . ord $ x) 3
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
        | otherwise = (y:) <$> extractString ys
tokenizeString _  = Nothing

tokenizeComment :: Tokenizer
tokenizeComment (';':xs) = return $ TokenizeResult (Comment comment) (length comment + 1)
  where comment = takeWhile (/='\n') xs
tokenizeComment _ = Nothing

type SeparatorPredicate = Char -> Bool
sepTokenizer :: SeparatorPredicate -> Tokenizer -> Tokenizer
sepTokenizer _ _ [] = Nothing
sepTokenizer predicate tokenizer input = do
  result@(TokenizeResult _ consumed) <- tokenizer input
  let next = drop consumed input
  guard $ null next || (predicate . head $ next)
  return result

anyTokenizer :: [Tokenizer] -> Tokenizer
anyTokenizer _ [] = Nothing
anyTokenizer tokenizers input = getFirst . mconcat . map First $ sequenceA tokenizers input

tokenize :: String -> Either String [Token]
tokenize [] = Right []
tokenize input = tokens >>= (\(TokenizeResult token chars) -> (token:) <$> tokenize (drop chars input)) >>= (Right . filter tokenFilter)
  where tokens = maybeToEither (tokenizers input) $ "Unknown token: " ++ take 20 input
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
tokenFilter WhiteSpace  = False
tokenFilter (Comment _) = False
tokenFilter _           = True
