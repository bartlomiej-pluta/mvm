module Assembler.Parser where

import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified VirtualMachine as VM
import qualified Assembler.Tokenizer as T
import qualified Util as U

data AST = Empty
         | Operator VM.Op 
         | Integer Int
         | Identifier String
         | Colon
         | Ampersand
         | LabelDef String
         | LabelRef String
         | Param AST
         | Params [AST]
         | Instruction AST AST
         | Line AST AST      
         | Program [AST]   
         deriving (Eq, Show)

type ConsumedTokens = Int
data ParseResult = ParseResult AST ConsumedTokens deriving (Eq, Show)

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
parseIdentifier ((T.Identifier id):_) = Just $ ParseResult (Identifier id) 1
parseIdentifier _                     = Nothing

-- ':'
parseColon :: Parser
parseColon ((T.Colon):_) = Just $ ParseResult Colon 1
parseColon _             = Nothing

-- '&'
parseAmpersand :: Parser
parseAmpersand ((T.Ampersand):_) = Just $ ParseResult Ampersand 1
parseAmpersand _                 = Nothing

-- label_def := ID ':'
parseLabelDef :: Parser
parseLabelDef = parseSeq [parseIdentifier, parseColon] combine
  where combine = (\[(Identifier id), _] -> LabelDef id)

-- label_ref := '&' ID
parseLabelRef :: Parser
parseLabelRef = parseSeq [parseAmpersand, parseIdentifier] combine
  where combine = (\[_, (Identifier id)] -> LabelRef id)

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
mapAST parser mapper tokens = do
  (ParseResult ast consumed) <- parser tokens
  return $ ParseResult (mapper ast) consumed

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
parseMany parser combiner tokens = if null asts
  then Nothing
  else Just $ ParseResult ast consumed
  where    
    results = parseGreedy parser tokens
    consumed = sum $ map (\(ParseResult _ c) -> c) results
    asts = map (\(ParseResult a _) -> a) results    
    ast = combiner asts

-- a a a a a a a...
parseGreedy :: Parser -> [T.Token] -> [ParseResult]
parseGreedy parser tokens = case parser tokens of
  (Just r@(ParseResult ast consumed)) -> r : parseGreedy parser (drop consumed tokens)
  Nothing                             -> []

-- a | b | c
parseAlt :: [Parser] -> (AST -> AST) -> Parser
parseAlt parsers mapper tokens = do
  (ParseResult ast consumed) <- parseAny parsers tokens
  return $ ParseResult (mapper ast) consumed

-- a | b | c
parseAny :: [Parser] -> Parser
parseAny _ [] = Nothing
parseAny parsers tokens = Monoid.getFirst . Monoid.mconcat . map Monoid.First $ sequenceA parsers tokens

-- a b c
parseSeq :: [Parser] -> ([AST] -> AST) -> Parser
parseSeq _ _ [] = Nothing
parseSeq parsers combiner tokens = do
  results <- parseAll parsers tokens
  let consumed = sum $ map (\(ParseResult _ c) -> c) results
  let asts = map (\(ParseResult a _) -> a) results
  if (length asts) == (length parsers)
  then return $ ParseResult (combiner asts) consumed
  else Nothing

-- a b c
parseAll :: [Parser] -> [T.Token] -> Maybe [ParseResult]
parseAll [] _ = Just []
parseAll (p:ps) tokens = do
  (ParseResult ast consumed) <- p tokens
  rest <- parseAll ps (drop consumed tokens)
  return $ (ParseResult ast consumed) : rest

-- 'Nothing' if not consumed tokens exist
assertConsumed :: Parser -> Parser
assertConsumed _ [] = Nothing
assertConsumed parser tokens = do
  r@(ParseResult _ consumed) <- parser tokens
  if null (drop consumed tokens)
  then return r
  else Nothing

parse :: [T.Token] -> Either String AST
parse tokens = do
  let lines = U.explode (==T.NewLine) tokens
  let results = map (assertConsumed parseLine) lines
  let errors = filter ((==Nothing) . snd) $ zipWith (,) lines $ results 
  let errorMsg = "Parse error(s):\n" ++ (List.intercalate "\n" $ map (show . fst) errors)
  case sequenceA results of
    (Just r) -> return $ Program $ map (\(ParseResult ast _) -> ast) r
    Nothing  -> Left errorMsg