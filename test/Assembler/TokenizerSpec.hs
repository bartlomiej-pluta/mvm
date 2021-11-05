module Assembler.TokenizerSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Numeric (showHex)
import Data.Char (ord)

import Assembler.Tokenizer
import VirtualMachine

success :: Token -> Int -> Maybe TokenizeResult
success token consumed = Just $ TokenizeResult token consumed

spec :: Spec
spec = do
  describe "keywordTokenizer" $ do 
    it "supports truncated input" $
      keywordTokenizer True "hey" NewLine "hey" `shouldBe` success NewLine 3
    it "supports non-truncated input" $
      keywordTokenizer True "hey" NewLine "heyjude" `shouldBe` success NewLine 3
    it "supports case sensitivity" $
      keywordTokenizer True "hey" NewLine "heYjude" `shouldBe` Nothing
    it "supports case insensitivity" $
      keywordTokenizer False "hey" NewLine "heYjude" `shouldBe` success NewLine 3    
    it "returns correct token" $
      keywordTokenizer True "hey" Colon "heyjude" `shouldBe` success Colon 3
    it "returns Nothing if input does not match" $
      keywordTokenizer True "hey" Colon "xheyjude" `shouldBe` Nothing
    it "supports empty input" $ 
      keywordTokenizer True "hey" Colon "" `shouldBe` Nothing

  describe "operatorTokenizer" $ do
    it "returns proper operator when given a truncated input" $
      operatorTokenizer Push "push" `shouldBe` success (Operator Push) 4
    it "returns proper operator when given a nontruncated input" $
      operatorTokenizer Pop "pops" `shouldBe` success (Operator Pop) 3
    it "returns Nothing if input does not match" $
      operatorTokenizer Pop "poop" `shouldBe` Nothing
    it "supports empty input" $ 
      operatorTokenizer Call "" `shouldBe` Nothing

  describe "tokenizeOperators" $ do
    it "autodetects and returns proper operators" $ do
      let ops = [Nop ..]
      let input = map show ops
      let expected = map (\o -> success (Operator o) (length . show $ o)) ops
      map tokenizeOperators input `shouldBe` expected
    it "is case insensitive" $ do
      let input = ["jmp", "CALL", "pUsH"]
      let expected = [ success (Operator Jmp) 3
                   , success (Operator Call) 4
                   , success (Operator Push) 4
                   ]
      map tokenizeOperators input `shouldBe` expected
    it "rejects other input" $
      tokenizeOperators "some unsupported input" `shouldBe` Nothing
    it "supports empty input" $ 
      tokenizeOperators "" `shouldBe` Nothing

  describe "tokenizeIdentifier" $ do
    it "parses correct identifier" $
      tokenizeIdentifier "someId" `shouldBe` success (Identifier "someId") 6
    it "parses correct identifier with numbers" $
      tokenizeIdentifier "someId14" `shouldBe` success (Identifier "someId14") 8
    it "parses correct identifier with underscores" $
      tokenizeIdentifier "some_Id" `shouldBe` success (Identifier "some_Id") 7      
    it "disallows to start identifier with underscore" $    
      tokenizeIdentifier "_someId" `shouldBe` Nothing
    it "disallows to start identifier with digit" $    
      tokenizeIdentifier "5someId" `shouldBe` Nothing
    it "supports empty input" $ 
      tokenizeIdentifier "" `shouldBe` Nothing

  describe "tokenizeWhitespace" $ do
    it "parses space" $
      tokenizeWhitespace " " `shouldBe` success WhiteSpace 1
    it "parses tab" $
      tokenizeWhitespace "\t" `shouldBe` success WhiteSpace 1
    it "parses newline" $
      tokenizeWhitespace "\n" `shouldBe` success WhiteSpace 1
    it "parses CR" $
      tokenizeWhitespace "\r" `shouldBe` success WhiteSpace 1
    it "rejects non-whitespace chars" $ do
      let input = map (\x -> [x]) $ ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']
      let expected = take (length input) . repeat $ Nothing
      map tokenizeWhitespace input `shouldBe` expected
    it "supports empty input" $ 
      tokenizeIdentifier "" `shouldBe` Nothing      
  
  describe "tokenizeDecimal" $ do
    it "parses numbers from 0 to 65535" $ do
      let nums = [0 .. 65535]
      let input = map show nums
      let expected = map (\n -> success (IntLiteral n) (length . show $ n)) nums
      map tokenizeDecimal input `shouldBe` expected
    it "does not support negative numbers" $ do
      let nums = [-1, -2 .. -65535] :: [Integer]
      let input = map show nums
      let expected = take (length nums) . repeat $ Nothing
      map tokenizeDecimal input `shouldBe` expected
    it "rejects other input" $
      tokenizeDecimal "some unsupported input" `shouldBe` Nothing   
    it "supports empty input" $ 
      tokenizeDecimal "" `shouldBe` Nothing             
    
  describe "tokenizeHex" $ do
    it "parses numbers from 0x0 to 0xFFFF" $ do
      let nums = [0 .. 0xFFFF]
      let input = map (("0x"++) . (flip showHex "")) nums
      let expected = map (\n -> success (IntLiteral n) (length . ("0x"++) . (flip showHex "") $ n)) nums
      map tokenizeHex input `shouldBe` expected
    it "does not support negative numbers" $ do
      let nums = [0 .. 0xFFFF] :: [Integer]
      let input = map (("-0x"++) . (flip showHex "")) nums
      let expected = take (length nums) . repeat $ Nothing
      map tokenizeHex input `shouldBe` expected      
    it "accepts left-padded number" $
      tokenizeHex "0x0010" `shouldBe` success (IntLiteral 16) 6
    it "rejects other input" $
      tokenizeHex "some unsupported input" `shouldBe` Nothing   
    it "rejects '0'" $
      tokenizeHex "0" `shouldBe` Nothing
    it "rejects '0x'" $
      tokenizeHex "0x" `shouldBe` Nothing      
    it "supports empty input" $ 
      tokenizeHex "" `shouldBe` Nothing   

  describe "tokenizeChar" $ do
    it "parses letters literals" $ do
      let chars = ['a' .. 'z'] ++ ['A' .. 'Z']
      let input = map (\c -> "'" ++ [c] ++ "'") chars
      let expected = map (\c -> success (IntLiteral (ord c)) 3) chars
      map tokenizeChar input `shouldBe` expected
    it "parses digits literals" $ do
      let chars = ['0' .. '9']
      let input = map (\c -> "'" ++ [c] ++ "'") chars
      let expected = map (\c -> success (IntLiteral (ord c)) 3) chars
      map tokenizeChar input `shouldBe` expected      
    it "parses regular symbols literals" $ do
      let chars = "!@#$%^&*()_+-=[]{};:|,/?<>\""
      let input = map (\c -> "'" ++ [c] ++ "'") chars
      let expected = map (\c -> success (IntLiteral (ord c)) 3) chars
      map tokenizeChar input `shouldBe` expected    
    it "parses escape sequences literals" $ do      
      let input = [ "'\\n'"
                  , "'\\t'"
                  , "'\\v'"
                  , "'\\b'"
                  , "'\\r'"
                  , "'\\f'"
                  , "'\\a'"
                  , "'\\\\'"
                  , "'\\''"
                  , "'\\0'"
                  ]
      let expected = map (flip success 4 . IntLiteral) [10, 9, 11, 8, 13, 12, 7, 92, 39, 0]
      map tokenizeChar input `shouldBe` expected
    it "consumes double-quote without escaping" $
      tokenizeChar "'\"'" `shouldBe` success (IntLiteral 34) 3
    it "consumes double-quote with escaping" $
      tokenizeChar "'\\\"'" `shouldBe` success (IntLiteral 34) 4
    it "rejects non-single char literals" $
      tokenizeChar "'ab'" `shouldBe` Nothing
    it "rejects non-closed char literals" $
      tokenizeChar "'a" `shouldBe` Nothing
    it "rejects invalid escape sequences" $ 
      tokenizeChar "'\\x'" `shouldBe` Nothing
    it "rejects empty quotes" $
      tokenizeChar "''" `shouldBe` Nothing
    it "rejects single quote" $
      tokenizeChar "'" `shouldBe` Nothing
    it "supports empty input" $
      tokenizeChar "" `shouldBe` Nothing

  describe "tokenizeString" $ do
    it "parses correct string" $
      tokenizeString "\"Hello, world!\"" `shouldBe` success (StringLiteral "Hello, world!") 15
    it "parses empty string" $
      tokenizeString "\"\"" `shouldBe` success (StringLiteral "") 2
    it "supports digits in strings" $ do
      let str = "34823843dedsef231542c4f324c24234ffsef234g"
      let len = length str + 2
      let input = "\"" ++ str ++ "\""
      tokenizeString input `shouldBe` success (StringLiteral str) len
    it "supports regular symbols in strings" $ do
      let str = "!@2#$%9^&*(1)_s+2-=[2h6sh]t{};:'e|<>,./?"
      let len = length str + 2
      let input = "\"" ++ str ++ "\""
      tokenizeString input `shouldBe` success (StringLiteral str) len  
    it "supports escape sequences literals" $ do
      pendingWith "We need probably to fix tokenizeString since the following test fails"
      -- TODO:
      -- let str = "\\n\\t\\v\\b\\r\\f\\a\\\\\\\"\\0"
      -- let len = length str + 2
      -- let input = "\"" ++ str ++ "\""
      -- let expected = map chr [10, 9, 11, 8, 13, 12, 7, 92, 34, 0]
      -- tokenizeString input `shouldBe` success (StringLiteral expected) len  
    it "consumes single quotes without escaping" $
      tokenizeString "\"'''''''''\"" `shouldBe` success (StringLiteral "'''''''''") 11
    it "consumes single quotes with escaping " $
      tokenizeString "\"\\'\\'\\'\\'\\'\\'\"" `shouldBe` success (StringLiteral "''''''") 14
    it "rejects invalid escape sequences" $
      tokenizeString "\"\\x\"" `shouldBe` Nothing
    it "rejects non-closed string literals" $
      tokenizeString "\"hello, world!" `shouldBe` Nothing
    it "rejects single double-quote" $
      tokenizeString "\"" `shouldBe` Nothing
    it "rejects multilined strings" $
      tokenizeString "\"first line\nsecond line\"" `shouldBe` Nothing
    it "supports empty input" $
      tokenizeString "" `shouldBe` Nothing      
    
  describe "tokenizeComment" $ do
    it "properly consumes comment" $ 
      tokenizeComment ";some comment\n" `shouldBe` success (Comment "some comment") 13
    it "properly consumes comment with whitespace padding" $ 
      tokenizeComment ";  \t  some comment  \t \n  \t" `shouldBe` success (Comment "  \t  some comment  \t ") 22    
    it "does not treat the input as a comment if it does not start with semicolon" $
      tokenizeComment "some not valid comment\n" `shouldBe` Nothing
    it "expands the comment till the end of the line" $ 
      tokenizeComment "; some comment ; push 4 push 10\nadd" `shouldBe` success (Comment " some comment ; push 4 push 10") 31
    it "parses the comment at the end of the input" $ 
      tokenizeComment "; some comment " `shouldBe` success (Comment " some comment ") 15
    it "supports empty input" $
      tokenizeComment "" `shouldBe` Nothing

  describe "sepTokenizer" $ do
    it "produces the token only when the separator is present" $ do
      let input = "abc-"
      let expected = success Colon 3
      let tokenizer _ = expected
      sepTokenizer ('-'==) tokenizer input `shouldBe` expected
    it "produces the token if its the end of input even if separator is not present" $ do
      let input = "abc"
      let expected = success Colon 3
      let tokenizer _ = expected
      sepTokenizer ('-'==) tokenizer input `shouldBe` expected
    it "does not produce any token when the separator is not present" $ do
      let input = "abcd"
      let tokenizer _ = success Colon 3
      sepTokenizer ('-'==) tokenizer input `shouldBe` Nothing
    it "does not produce any token when the space is present instead" $ do
      let input = "abc "
      let tokenizer _ = success Colon 3
      sepTokenizer ('-'==) tokenizer input `shouldBe` Nothing     
    it "does not change the number of consumed chars even though it's checking the separator presence" $ do
      let input = "abc-"
      let expected = success Colon 3
      let tokenizer _ = expected
      let (Just (TokenizeResult _ consumedChars)) = sepTokenizer ('-'==) tokenizer input
      consumedChars `shouldBe` 3
    it "supports empty input irrespective of wrapped tokenizer" $ do
      let input = ""
      let tokenizer _ = success Colon 3 -- MOCK: tokenizer returns Just even though the input is empty
      sepTokenizer ('-'==) tokenizer input `shouldBe` Nothing  
    
  describe "anyTokenizer" $ do
    it "returns the token if at least one subtokenizer produce that" $ do
      let values = [ success Ampersand 1
                   , Nothing
                   , Nothing
                   , Nothing
                   , Nothing 
                   ]
      let t = map (\x -> (\_ -> x)) values
      anyTokenizer t "some not important input" `shouldBe` success Ampersand 1
    it "returns the token of the first matching subtokenizer" $ do
      let values = [ Nothing
                   , Nothing
                   , success (IntLiteral 4) 1
                   , Nothing
                   , Nothing 
                   , success (StringLiteral "not me") 8
                   , Nothing
                   , success (StringLiteral "me neither") 12
                   , Nothing
                   , success Ampersand 1
                   , Nothing
                   , Nothing
                   , success Colon 1
                   , Nothing
                   ]
      let t = map (\x -> (\_ -> x)) values
      anyTokenizer t "some not important input" `shouldBe` success (IntLiteral 4) 1
    it "returns Nothing if no one of the tokenizers matches the input" $ do
      let values = [ Nothing
                   , Nothing
                   , Nothing
                   , Nothing
                   , Nothing 
                   ]
      let t = map (\x -> (\_ -> x)) values
      anyTokenizer t "some not important input" `shouldBe` Nothing
    it "always returns Nothing if no tokenizers are defined" $
      anyTokenizer [] "some not important input" `shouldBe` Nothing  
    it "supports empty input irrespective of wrapped tokenizers" $ do
      let input = ""
      let values = [ success Ampersand 1
                   , success Colon 1
                   , success (IntLiteral 3) 1
                   , success (Operator Push) 4
                   ]
      let t = map (\x -> (\_ -> x)) values
      anyTokenizer t input `shouldBe` Nothing     

  describe "tokenFilter" $ do
    it "filters out whitespaces and comments" $ do
      let tokens = [ Operator Push
                   , IntLiteral 4
                   , Comment "here is the identifier"                   
                   , Identifier "someId"
                   , WhiteSpace                   
                   , Colon
                   , WhiteSpace                   
                   , Ampersand
                   , NewLine
                   , WhiteSpace
                   , Comment "some comment"                   
                   ]
      let expected = [ Operator Push
                     , IntLiteral 4
                     , Identifier "someId"
                     , Colon
                     , Ampersand
                     , NewLine
                     ]                   
      filter tokenFilter tokens `shouldBe` expected

  describe "tokenize" $ do
    it "treats 'pop' as a operator instead of identifier" $
      tokenize "pop" `shouldBe` Right [Operator Pop]
    it "treats 'poop' as a identifier" $
      tokenize "poop" `shouldBe` Right [Identifier "poop"] 
    it "treats operator as themselves instead of identifiers" $ do
      let ops = [Nop ..]
      let input = map show ops
      let expected = map (\o -> Right [Operator o]) ops 
      map tokenize input `shouldBe` expected
    it "treats operator-like names (with 's' appended) as identifiers" $ do
      let ops = [Nop ..]
      let input = map ((++"s") . show) ops
      let expected = map (\i-> Right [Identifier i]) input 
      map tokenize input `shouldBe` expected      
    it "treats '\n' as a newline instead of whitespace" $
      tokenize "\n" `shouldBe` Right [NewLine]
    it "ignores comments" $ do
      let input = "; this is some comment \n\
                 \ ; this is another comment"
      tokenize input `shouldBe` Right [NewLine]
    it "interprets 'push 5\npush5' as operator, int literal, NL and identifier" $ do
      let input = "push 5\npush5"
      let expected = [Operator Push, IntLiteral 5, NewLine, Identifier "push5"]
      tokenize input `shouldBe` Right expected
    it "treats 'someId1234' as 'someId1234' identifier instead of 'someId' identifier and 1234 int" $
      tokenize "someId1234" `shouldBe` Right [Identifier "someId1234"]
    it "accepts 'main: NL" $
      tokenize "main: \n" `shouldBe` Right [Identifier "main", Colon, NewLine]
    it "accepts 'call &sum NL" $
      tokenize "call &sum \n" `shouldBe` Right [Operator Call, Ampersand, Identifier "sum", NewLine]      
    it "rejects '4push'" $
      tokenize "4push" `shouldBe` Left "Unknown token: 4push"  
    it "supports empty input" $ 
      tokenize "" `shouldBe` Right []  
    it "interprets example #1" $ do
      let input = "main: ; here we define some main label\n\
                \      push 7    ; we push 7 to the stack\n\
                \      push 0x04 ; we push 4 to the stack\n\
                \      call &sum ; we call 'sum' subprogram\n\
                \      halt \n\
                \ \n\
                \ sum: add\n\
                \      ret"
      let expected = [ Identifier "main", Colon, NewLine
                     , Operator Push, IntLiteral 7, NewLine
                     , Operator Push, IntLiteral 4, NewLine
                     , Operator Call, Ampersand, Identifier "sum", NewLine
                     , Operator Halt, NewLine
                     , NewLine
                     , Identifier "sum", Colon, Operator Add, NewLine
                     , Operator Ret
                     ]    
      tokenize input `shouldBe` Right expected        