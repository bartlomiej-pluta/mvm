{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Assembler.Parser where

import Data.List (intercalate)
import Data.Monoid (First(..))

import qualified Assembler.Tokenizer as T (Token(..))
import VirtualMachine.VM (Op)
import Util (explode, maybeToEither)
import Control.Monad (guard)

data Scope = Local | Global deriving (Eq, Show, Enum, Bounded)

data AST = Empty
         | Operator Op
         | Integer Int
         | Identifier String
         | Colon
         | Ampersand
         | Dot
         | LabelDef Scope String
         | LabelRef Scope String
         | Param AST
         | Params [AST]
         | Instruction AST AST
         | Line AST AST
         | Program [AST]
         deriving (Eq, Show)

type ConsumedTokens = Int
data ParseResult = ParseResult { _ast :: AST
                               , _consumed :: ConsumedTokens
                               } deriving (Eq, Show)

type Parser = [T.Token] -> Maybe ParseResult

-- OP := push | pop ...
parseOperator :: Parser
parseOperator ((T.Operator op):_) = Just $ ParseResult (Operator op) 1
parseOperator _                   = Nothing

-- INT := 0 | 1 | ... | 0x00 | 0x01 | ... | 'a' | 'b' | ...
parseInt :: Parser
parseInt ((T.IntLiteral int):_) = Just $ ParseResult (Integer int) 1
parseInt _                      = Nothing

-- ID := [alnum, '_']+
parseIdentifier :: Parser
parseIdentifier ((T.Identifier iden):_) = Just $ ParseResult (Identifier iden) 1
parseIdentifier _                       = Nothing

-- ':'
parseColon :: Parser
parseColon (T.Colon:_) = Just $ ParseResult Colon 1
parseColon _           = Nothing

-- '&'
parseAmpersand :: Parser
parseAmpersand (T.Ampersand:_) = Just $ ParseResult Ampersand 1
parseAmpersand _               = Nothing

-- '.'
parseDot :: Parser
parseDot (T.Dot:_) = Just $ ParseResult Dot 1
parseDot _         = Nothing

-- label_def := '.'? ID ':'
parseLabelDef :: Parser
parseLabelDef = parseSeq [parseOptionally parseDot, parseIdentifier, parseColon] combine
  where combine [Dot, Identifier iden, _] = LabelDef Local iden
        combine [_,   Identifier iden, _] = LabelDef Global iden

-- label_ref := '&' ID
parseLabelRef :: Parser
parseLabelRef = parseSeq [parseAmpersand, parseOptionally parseDot, parseIdentifier] combine
  where combine [_, Dot, Identifier iden] = LabelRef Local iden
        combine [_, _,   Identifier iden] = LabelRef Global iden

-- param := INT | label_ref
parseParam :: Parser
parseParam = parseAlt [parseInt, parseLabelRef] Param

-- instr := OP param*
parseInstr :: Parser
parseInstr = parseSeq [parseOperator, parseMany0 parseParam Params] (\[op, ps] -> Instruction op ps)

-- line := label_def? instr?
parseLine :: Parser
parseLine = parseSeq [parseOptionally parseLabelDef, parseOptionally parseInstr] (\[label, instr] -> Line label instr)

mapAST :: Parser -> (AST -> AST) -> Parser
mapAST _ _ [] = Nothing
mapAST parser mapper tokens = parser tokens >>= \(ParseResult ast consumed) -> return $ ParseResult (mapper ast) consumed

-- a?
parseOptionally :: Parser -> Parser
parseOptionally parser input = case parser input of
  Nothing -> Just $ ParseResult Empty 0
  result  -> result

-- a*
parseMany0 :: Parser -> ([AST] -> AST) -> Parser
parseMany0 parser combiner = parseOptionally $ parseMany parser combiner

-- a+
parseMany :: Parser -> ([AST] -> AST) -> Parser
parseMany parser combiner tokens = guard (not $ null asts) >> return (ParseResult ast consumed)
  where results  = parseGreedy parser tokens
        consumed = sum $ map _consumed results
        asts     = map _ast results
        ast      = combiner asts

-- a a a a a a a...
parseGreedy :: Parser -> [T.Token] -> [ParseResult]
parseGreedy parser tokens = case parser tokens of
  (Just r@(ParseResult _ consumed)) -> r : parseGreedy parser (drop consumed tokens)
  Nothing                           -> []

-- a | b | c
parseAlt :: [Parser] -> (AST -> AST) -> Parser
parseAlt parsers mapper tokens = do
  (ParseResult ast consumed) <- parseAny parsers tokens
  return $ ParseResult (mapper ast) consumed

-- a | b | c
parseAny :: [Parser] -> Parser
parseAny _ [] = Nothing
parseAny parsers tokens = getFirst . mconcat . map First $ sequenceA parsers tokens

-- a b c
parseSeq :: [Parser] -> ([AST] -> AST) -> Parser
parseSeq _ _ [] = Nothing
parseSeq parsers combiner tokens = do
  results <- parseAll parsers tokens
  let consumed = sum $ map _consumed results
  let asts = map _ast results
  guard $ length asts == length parsers
  return $ ParseResult (combiner asts) consumed

-- a b c
parseAll :: [Parser] -> [T.Token] -> Maybe [ParseResult]
parseAll [] _ = Just []
parseAll (p:ps) tokens = do
  (ParseResult ast consumed) <- p tokens
  rest <- parseAll ps (drop consumed tokens)
  return $ ParseResult ast consumed : rest

-- 'Nothing' if not consumed tokens exist
assertConsumed :: Parser -> Parser
assertConsumed _ [] = Nothing
assertConsumed parser tokens = parser tokens >>= \r@(ParseResult _ consumed) -> guard (null $ drop consumed tokens) >> return r

parse :: [T.Token] -> Either String AST
parse tokens = do
  let codeLines = explode (==T.NewLine) tokens
  let results = map (assertConsumed parseLine) codeLines
  let errors = filter ((==Nothing) . snd) $ zip codeLines results
  let errorMsg = "Parse error(s):\n" ++ intercalate "\n" (map (show . fst) errors)
  Program . map _ast <$> maybeToEither (sequenceA results) errorMsg