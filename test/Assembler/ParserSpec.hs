module Assembler.ParserSpec where

import Test.Hspec

import qualified Assembler.Tokenizer as T
import Assembler.Parser
import VirtualMachine

success :: AST -> Int -> Maybe ParseResult
success ast consumed = Just $ ParseResult ast consumed

spec :: Spec
spec = do
  describe "parseOperator" $ do
    it "accepts operator tokens" $ do
      let ops = [Nop ..]
      let input = map ((:[]) . T.Operator) ops
      let expected = map (flip success 1 . Operator) ops
      map parseOperator input `shouldBe` expected
    it "supports non-truncated input" $ do
      parseOperator [T.Operator Call, T.Ampersand, T.Identifier "label"] `shouldBe` success (Operator Call) 1      
    it "supports empty input" $
      parseOperator [] `shouldBe` Nothing

  describe "parseInt" $ do
    it "accepts int literal tokens" $ do
      let ints = [-65535, -65534 .. 65535]
      let input = map ((:[]) . T.IntLiteral) ints
      let expected = map (flip success 1 . Integer) ints
      map parseInt input `shouldBe` expected
    it "supports non-truncated input" $ do
      parseInt [T.IntLiteral 4, T.Colon] `shouldBe` success (Integer 4) 1
    it "supports empty input" $
      parseInt [] `shouldBe` Nothing

  describe "parseIdentifier" $ do
    it "accepts identifier tokens" $
      parseIdentifier [T.Identifier "someId"] `shouldBe` success (Identifier "someId") 1
    it "supports non-truncated input" $ do
      parseIdentifier [T.Identifier "label", T.Colon] `shouldBe` success (Identifier "label") 1      
    it "supports empty input" $
      parseIdentifier [] `shouldBe` Nothing

  describe "parseColon" $ do
    it "accepts colon tokens" $
      parseColon [T.Colon] `shouldBe` success Colon 1
    it "supports non-truncated input" $ do
      parseColon [T.Colon, T.Operator Add] `shouldBe` success Colon 1            
    it "supports empty input" $
      parseColon [] `shouldBe` Nothing

  describe "parseAmpersand" $ do
    it "accepts colon tokens" $
      parseAmpersand [T.Ampersand] `shouldBe` success Ampersand 1
    it "supports non-truncated input" $ do
      parseAmpersand [T.Ampersand, T.Identifier "label"] `shouldBe` success Ampersand 1          
    it "supports empty input" $
      parseAmpersand [] `shouldBe` Nothing      
  
  describe "parseLabelDef" $ do
    it "parses 'label:'" $
      parseLabelDef [T.Identifier "label", T.Colon] `shouldBe` success (LabelDef "label") 2
    it "requires label" $
      parseLabelDef [T.Colon] `shouldBe` Nothing
    it "requires colon" $
      parseLabelDef [T.Identifier "label"] `shouldBe` Nothing   
    it "supports non-truncated input" $ do
      parseLabelDef [T.Identifier "sum", T.Colon, T.Operator Nop] `shouldBe` success (LabelDef "sum") 2         
    it "supports empty input" $
      parseLabelDef [] `shouldBe` Nothing

  describe "parseLabelRef" $ do
    it "parses '&label'" $
      parseLabelRef [T.Ampersand, T.Identifier "label"] `shouldBe` success (LabelRef "label") 2
    it "requires label" $
      parseLabelRef [T.Ampersand] `shouldBe` Nothing
    it "requires ampersand" $
      parseLabelRef [T.Identifier "label"] `shouldBe` Nothing   
    it "supports non-truncated input" $ do
      parseLabelRef [T.Ampersand, T.Identifier "sum", T.Operator Nop] `shouldBe` success (LabelRef "sum") 2         
    it "supports empty input" $
      parseLabelRef [] `shouldBe` Nothing      

  describe "parseParam" $ do
    it "parses int params" $ do
      let ints = [-65535, -65534 .. 65535]
      let input = map ((:[]) . T.IntLiteral) ints
      let expected = map (flip success 1 . Param . Integer) ints
      map parseParam input `shouldBe` expected
    it "parses label references" $ do
      let expected = success (Param (LabelRef "program")) 2
      parseParam [T.Ampersand, T.Identifier "program"] `shouldBe` expected
    it "supports non-truncated input" $ do
      let expected = success (Param (Integer 1)) 1
      parseParam [T.IntLiteral 1, T.IntLiteral 2, T.IntLiteral 3] `shouldBe` expected  
    it "supports empty input" $
      parseParam [] `shouldBe` Nothing       

  describe "parseInstr" $ do
    it "parses no-param operator" $ do
      let input = [T.Operator Halt]
      let expected = success (Instruction
                               (Operator Halt)
                               Empty
                             ) (length input)
      parseInstr input `shouldBe` expected
    it "parses operator with single int param" $ do
      let input = [T.Operator Push, T.IntLiteral 4]
      let expected = success (Instruction
                               (Operator Push)
                               (Params [
                                (Param (Integer 4))
                               ])
                             ) (length input)
      parseInstr input `shouldBe` expected      
    it "parses operator with single label ref param" $ do
      let input = [T.Operator Call, T.Ampersand, T.Identifier "program"]
      let expected = success (Instruction
                               (Operator Call)
                               (Params [
                                (Param (LabelRef "program"))
                               ])
                             ) (length input)
      parseInstr input `shouldBe` expected    
    it "parses operator with multiple int params" $ do
      let input = [T.Operator Push
                  , T.IntLiteral 1
                  , T.IntLiteral 4
                  , T.IntLiteral 2
                  , T.IntLiteral 0
                  ]                
      let expected = success (Instruction
                               (Operator Push)
                               (Params [
                                (Param (Integer 1)),
                                (Param (Integer 4)),
                                (Param (Integer 2)),
                                (Param (Integer 0))
                               ])
                             ) (length input)
      parseInstr input `shouldBe` expected       
    it "parses operator with multiple param ref params" $ do
      let input = [T.Operator Push
                  , T.Ampersand, T.Identifier "program"
                  , T.Ampersand, T.Identifier "main"
                  , T.Ampersand, T.Identifier "foo"
                  , T.Ampersand, T.Identifier "bar"
                  ]                
      let expected = success (Instruction
                               (Operator Push)
                               (Params [
                                (Param (LabelRef "program")),
                                (Param (LabelRef "main")),
                                (Param (LabelRef "foo")),
                                (Param (LabelRef "bar"))
                               ])
                             ) (length input)
      parseInstr input `shouldBe` expected       
    it "parses operator with multiple mixed params" $ do
      let input = [T.Operator Push
                  , T.Ampersand, T.Identifier "program"
                  , T.IntLiteral 4
                  , T.Ampersand, T.Identifier "main"
                  , T.Ampersand, T.Identifier "foo"
                  , T.IntLiteral 10
                  , T.IntLiteral 11
                  , T.Ampersand, T.Identifier "bar"
                  , T.IntLiteral 20
                  ]                
      let expected = success (Instruction
                               (Operator Push)
                               (Params [
                                (Param (LabelRef "program")),
                                (Param (Integer 4)),
                                (Param (LabelRef "main")),
                                (Param (LabelRef "foo")),
                                (Param (Integer 10)),
                                (Param (Integer 11)),
                                (Param (LabelRef "bar")),
                                (Param (Integer 20))
                               ])
                             ) (length input)
      parseInstr input `shouldBe` expected 
    it "supports non-truncated input" $ do
      let input = [T.Operator Push
                  , T.Ampersand, T.Identifier "program"
                  , T.IntLiteral 4
                  , T.Ampersand, T.Identifier "main"
                  , T.Ampersand, T.Identifier "foo"
                  , T.IntLiteral 10
                  , T.IntLiteral 11
                  , T.Ampersand, T.Identifier "bar"
                  , T.IntLiteral 20 -- this is the last param, so we're going to stop here (13 tokens so far)
                  , T.Operator Call
                  , T.Ampersand, T.Identifier "program"                  
                  ]                
      let expected = success (Instruction
                               (Operator Push)
                               (Params [
                                (Param (LabelRef "program")),
                                (Param (Integer 4)),
                                (Param (LabelRef "main")),
                                (Param (LabelRef "foo")),
                                (Param (Integer 10)),
                                (Param (Integer 11)),
                                (Param (LabelRef "bar")),
                                (Param (Integer 20))
                               ])
                             ) 13
      parseInstr input `shouldBe` expected 
    it "supports empty input" $
      parseInstr [] `shouldBe` Nothing       

  describe "parseLine" $ do              
    it "supports label definition and operator in the same line" $ do
      let input = [T.Identifier "main", T.Colon, T.Operator Call, T.Ampersand, T.Identifier "program"]
      let expected = success (Line
                               (LabelDef "main")
                               (Instruction
                                (Operator Call)
                                (Params [
                                  (Param (LabelRef "program"))
                                ])
                               )
                             ) (length input)
      parseLine input `shouldBe` expected
    it "supports line with just label definition" $ do
      let input = [T.Identifier "main", T.Colon]
      let expected = success (Line
                               (LabelDef "main")     
                               Empty                          
                             ) (length input)
      parseLine input `shouldBe` expected      
    it "supports line with just operator" $ do
      let input = [T.Operator Call, T.Ampersand, T.Identifier "program"]
      let expected = success (Line
                               Empty
                               (Instruction
                                (Operator Call)
                                (Params [
                                  (Param (LabelRef "program"))
                                ])
                               )
                             ) (length input)
      parseLine input `shouldBe` expected
    it "supports non-truncated input" $ do
      let input = [T.Identifier "main", T.Colon, T.Operator Call, T.Ampersand, T.Identifier "program", T.Identifier "exit"]
      let expected = success (Line
                               (LabelDef "main")
                               (Instruction
                                (Operator Call)
                                (Params [
                                  (Param (LabelRef "program"))
                                ])
                               )
                             ) 5
      parseLine input `shouldBe` expected      
    it "parses empty input" $
      parseLine [] `shouldBe` Nothing
    
  describe "mapAST" $ do
    it "returns mapped AST if wrapped parser succeeded" $ do
      let astMapper ast = Param ast
      let parser = const $ success Colon 1
      let input = [T.StringLiteral "Some not important input"]
      mapAST parser astMapper input `shouldBe` success (Param Colon) 1
    it "results Nothing when wrapped parser failed" $ do
      let astMapper ast = Param ast
      let parser = const Nothing
      let input = [T.StringLiteral "Some not important input"]
      mapAST parser astMapper input `shouldBe` Nothing
    it "supports empty input irrespective of wrapped parser" $ do
      let astMapper ast = Param ast
      let parser = const $ success Colon 1
      let input = []
      mapAST parser astMapper input `shouldBe` Nothing
    
  describe "parseOptionally" $ do
    it "returns parsed AST if wrapped parser succeeded" $ do
      let parser = const $ success Ampersand 1
      let input = [T.StringLiteral "Some not important input"]
      parseOptionally parser input `shouldBe` success Ampersand 1
    it "returns Empty if wrapped parser failed" $ do
      let parser = const $ Nothing
      let input = [T.StringLiteral "Some not important input"]
      parseOptionally parser input `shouldBe` success Empty 0  
    it "supports empty input irrespective of wrapped parser" $ do
      let parser = const $ Nothing
      let input = []
      parseOptionally parser input `shouldBe` success Empty 0          

  describe "parseMany"  $ do
    it "parses many occurrences on truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon, T.Colon, T.Colon]
      parseMany colonParser combiner input `shouldBe` success (Params [Colon, Colon, Colon]) 3
    it "parses single occurence on truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon]
      parseMany colonParser combiner input `shouldBe` success (Params [Colon]) 1      
    it "parses many occurrences on non-truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon, T.Colon, T.Colon, T.Ampersand]
      parseMany colonParser combiner input `shouldBe` success (Params [Colon, Colon, Colon]) 3
    it "parses single occurence on non-truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon, T.Ampersand]
      parseMany colonParser combiner input `shouldBe` success (Params [Colon]) 1  
    it "rejects input if current token is not parseable" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Ampersand, T.Colon, T.Colon, T.Colon]
      parseMany colonParser combiner input `shouldBe` Nothing  
    it "supports empty input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = []
      parseMany colonParser combiner input `shouldBe` Nothing   

  describe "parseMany0"  $ do
    it "parses many occurrences on truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon, T.Colon, T.Colon]
      parseMany0 colonParser combiner input `shouldBe` success (Params [Colon, Colon, Colon]) 3
    it "parses single occurence on truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon]
      parseMany0 colonParser combiner input `shouldBe` success (Params [Colon]) 1      
    it "parses many occurrences on non-truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon, T.Colon, T.Colon, T.Ampersand]
      parseMany0 colonParser combiner input `shouldBe` success (Params [Colon, Colon, Colon]) 3
    it "parses single occurence on non-truncated input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Colon, T.Ampersand]
      parseMany0 colonParser combiner input `shouldBe` success (Params [Colon]) 1  
    it "accepts input even though current token is not parseable" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = [T.Ampersand, T.Colon, T.Colon, T.Colon]
      parseMany0 colonParser combiner input `shouldBe` success Empty 0  
    it "supports empty input" $ do
      let colonParser (T.Colon:_) = success Colon 1
          colonParser _           = Nothing   
      let combiner = Params
      let input = []
      parseMany0 colonParser combiner input `shouldBe` success Empty 0

  describe "parseAny" $ do
    it "returns the AST if at least one subparser produce that" $ do
      let parsers = map const [ success Ampersand 1
                              , Nothing
                              , Nothing
                              , Nothing
                              , Nothing 
                              ]
      let input = [T.StringLiteral "some not important input"]
      parseAny parsers input `shouldBe` success Ampersand 1
    it "returns the AST of the first matching subparser" $ do
      let parsers = map const [ Nothing
                              , Nothing
                              , success (Integer 4) 1
                              , Nothing
                              , Nothing 
                              , success (LabelDef "not me") 2
                              , Nothing
                              , success (Instruction (Operator Push) Empty) 1
                              , Nothing
                              , success Ampersand 1
                              , Nothing
                              , Nothing
                              , success Colon 1
                              , Nothing
                              ]
      let input = [T.StringLiteral "some not important input"]                              
      parseAny parsers input `shouldBe` success (Integer 4) 1
    it "returns Nothing if no one of the parsers matches the input" $ do
      let parsers = map const (take 4 $ repeat $ Nothing)
      let input = [T.StringLiteral "some not important input"]                              
      parseAny parsers input `shouldBe` Nothing
    it "always returns Nothing if no parsers are defined" $ do
      let input = [T.StringLiteral "some not important input"]                              
      parseAny [] input `shouldBe` Nothing
    it "supports empty input irrespective of wrapped parsers" $ do
      let parsers = map const [ success (Integer 4) 1
                              , success (LabelDef "not me") 2
                              , success (Instruction (Operator Push) Empty) 1
                              , Nothing
                              , success Ampersand 1
                              , success Colon 1
                              ]
      let input = []
      parseAny parsers input `shouldBe` Nothing  
              
  describe "parseSeq" $ do
    it "parses truncated input" $ do
      let colonParser (T.Colon:_)           = success Colon 1
          colonParser _                     = Nothing  
      let ampersandParser (T.Ampersand:_)   = success Ampersand 1
          ampersandParser _                 = Nothing              
      let combiner = Params
      let pattern = [colonParser, ampersandParser]
      let input = [T.Colon, T.Ampersand]
      parseSeq pattern combiner input `shouldBe` success (Params [Colon, Ampersand]) 2
    it "parses non-truncated input" $ do
      let colonParser (T.Colon:_)           = success Colon 1
          colonParser _                     = Nothing  
      let ampersandParser (T.Ampersand:_)   = success Ampersand 1
          ampersandParser _                 = Nothing              
      let combiner = Params
      let pattern = [colonParser, ampersandParser]
      let input = [T.Colon, T.Ampersand, T.Colon]
      parseSeq pattern combiner input `shouldBe` success (Params [Colon, Ampersand]) 2
    it "rejects incomplete pattern" $ do
      let colonParser (T.Colon:_)           = success Colon 1
          colonParser _                     = Nothing  
      let ampersandParser (T.Ampersand:_)   = success Ampersand 1
          ampersandParser _                 = Nothing              
      let combiner = Params
      let pattern = [colonParser, ampersandParser]
      let input = [T.Colon]
      parseSeq pattern combiner input `shouldBe` Nothing   
    it "rejects non-matching input" $ do
      let colonParser (T.Colon:_)           = success Colon 1
          colonParser _                     = Nothing  
      let ampersandParser (T.Ampersand:_)   = success Ampersand 1
          ampersandParser _                 = Nothing              
      let combiner = Params
      let pattern = [colonParser, ampersandParser]
      let input = [T.Ampersand, T.Colon]
      parseSeq pattern combiner input `shouldBe` Nothing    
    it "supports empty input irrespective of wrapped parsers" $ do
      let pattern = map const [ success (Integer 4) 1
                              , success (LabelDef "not me") 2
                              , success (Instruction (Operator Push) Empty) 1
                              , success Ampersand 1
                              , success Colon 1
                              ]
      let combiner = Params
      let input = []
      parseSeq pattern combiner input `shouldBe` Nothing

  describe "assertConsumed" $ do
    it "returns AST if no tokens are left to be consumed" $ do
      let parser = const $ success Colon 1
      let input = [T.Colon]
      assertConsumed parser input `shouldBe` success Colon 1
    it "returns Nothing if there are tokens left to be consumed, even though the wrapped parser succeeded to parse" $ do
      let parser = const $ success Colon 1
      let input = [T.Colon, T.Ampersand]
      assertConsumed parser input `shouldBe` Nothing  
    it "supports empty input" $ do
      let parser = const $ success Colon 1
      let input = []
      assertConsumed parser input `shouldBe` Nothing  

  describe "parse" $ do
    it "parses empty input" $ do
      let input = ""
      let (Right tokens) = T.tokenize input
      parse tokens `shouldBe` (Right $ Program [] :: Either String AST)
    it "parses line by line" $ do
      let input = "add1_2: push 1\npush 2\nadd"
      let (Right tokens) = T.tokenize input
      --                       Labels:                   Operations:                  Params:
      let expected = Program [ (Line (LabelDef "add1_2") (Instruction (Operator Push) (Params [Param $ Integer 1])))
                             , (Line Empty               (Instruction (Operator Push) (Params [Param $ Integer 2])))
                             , (Line Empty               (Instruction (Operator Add)  Empty))
                             ]
      parse tokens `shouldBe` (Right $ expected :: Either String AST)
    it "rejects multiple instructions in single line" $ do
      let input = "push 1 add"
      let (Right tokens) = T.tokenize input
      parse tokens `shouldBe` (Left "Parse error(s):\n[Operator Push,IntLiteral 1,Operator Add]" :: Either String AST)
    it "rejects multiple label definitions in single line" $ do
      let input = "label1: label2:"
      let (Right tokens) = T.tokenize input
      parse tokens `shouldBe` (Left "Parse error(s):\n[Identifier \"label1\",Colon,Identifier \"label2\",Colon]" :: Either String AST)
    it "rejects instruction followed by a label definition" $ do
      let input = "pop label:"      
      let (Right tokens) = T.tokenize input
      parse tokens `shouldBe` (Left "Parse error(s):\n[Operator Pop,Identifier \"label\",Colon]" :: Either String AST)      
    it "rejects orphaned identifiers" $ do
      let inputs = ["id", "push id", "main: id", "id main:"]
      let tokens = map ((\(Right t) -> t) . T.tokenize) inputs
      let expected = map Left [ "Parse error(s):\n[Identifier \"id\"]"
                              , "Parse error(s):\n[Operator Push,Identifier \"id\"]"
                              , "Parse error(s):\n[Identifier \"main\",Colon,Identifier \"id\"]"
                              , "Parse error(s):\n[Identifier \"id\",Identifier \"main\",Colon]"
                              ] :: [Either String AST]
      map parse tokens `shouldBe` expected
    it "rejects orphaned integers" $ do
      let inputs = ["1", "2 :", "3 push", "&4", "label 5 :"]
      let tokens = map ((\(Right t) -> t) . T.tokenize) inputs
      let expected = map Left [ "Parse error(s):\n[IntLiteral 1]"
                              , "Parse error(s):\n[IntLiteral 2,Colon]"
                              , "Parse error(s):\n[IntLiteral 3,Operator Push]"
                              , "Parse error(s):\n[Ampersand,IntLiteral 4]"
                              , "Parse error(s):\n[Identifier \"label\",IntLiteral 5,Colon]"
                              ] :: [Either String AST]
      map parse tokens `shouldBe` expected   
    it "parses example #1" $ do
      let input = "main: ; here we define some main label\n\
                \      push 7    ; we push 7 to the stack\n\
                \      push 0x04 ; we push 4 to the stack\n\
                \      call &sum ; we call 'sum' subprogram\n\
                \      halt \n\
                \ \n\
                \ sum: add\n\
                \      ret"
      let (Right tokens) = T.tokenize input
      --                       Labels:                         Operations:                  Params:
      let expected = Program [ (Line (LabelDef "main") Empty)
                             , (Line Empty                    (Instruction (Operator Push)  (Params [Param $ Integer 7])))
                             , (Line Empty                    (Instruction (Operator Push)  (Params [Param $ Integer 4])))
                             , (Line Empty                    (Instruction (Operator Call)  (Params [Param $ LabelRef "sum"])))
                             , (Line Empty                    (Instruction (Operator Halt)  Empty))
                             , (Line (LabelDef "sum")         (Instruction (Operator Add)   Empty))
                             , (Line Empty                    (Instruction (Operator Ret)   Empty))
                             ]
      parse tokens `shouldBe` (Right $ expected :: Either String AST)