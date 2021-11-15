module Assembler.EmitterSpec where

import Test.Hspec

import qualified Data.Map as M
import Control.Monad.Trans (lift)
import Control.Monad.State (evalState, get)
import Control.Monad.Trans.Except (runExceptT)

import Assembler.Tokenizer (tokenize)
import Assembler.Parser (AST(..), parse)
import Assembler.Emitter as E
import VirtualMachine.VM (Op(..))

evalContext :: Context -> AST -> Emitter -> Either String Context
evalContext ctx ast emitter = flip evalState ctx $ runExceptT $ emitter ast >> lift get 

spec :: Spec
spec = do
  describe "emitLabelDef" $ do
    it "inserts label definition to the context" $ do
      let ctx = E.empty
      let input = LabelDef "main"
      let expected = Right (ctx { _labels = M.fromList[("main", 0)] })
      evalContext ctx input emitLabelDef `shouldBe` expected
    it "does not allow to redefine label" $ do
      let ctx = E.empty { _labels = M.fromList [("main", 0)] }
      let input = LabelDef "main"
      let expected = Left "Label 'main' is already defined"
      evalContext ctx input emitLabelDef `shouldBe` expected
      
  describe "resolveLabels" $ do
    it "replaces reference with actual byte number" $ do
      let beans = [ Byte 1, Byte 2, Reference "main", Byte 4 ]
      let labels = M.fromList [("main", 3)]
      resolveLabels labels beans `shouldBe` Right [Byte 1, Byte 2, Byte 3, Byte 4]
    it "throws error if label does not exist" $ do
      let beans = [ Byte 1, Byte 2, Reference "not_existing_label", Byte 4 ]
      let labels = M.fromList [("main", 3)]
      let expected = Left "Label 'not_existing_label' is not defined"
      resolveLabels labels beans `shouldBe` expected

  describe "emitParam" $ do
    it "emits byte for integer literal" $ do
      let ctx = E.empty
      let input = Param (Integer 4)
      let expected = Right (ctx { _beans = [Byte 0x04] })
      evalContext ctx input emitParam `shouldBe` expected
    it "emits reference mark for label reference" $ do
      let ctx = E.empty
      let input = Param (LabelRef "main")
      let expected = Right (ctx { _beans = [Reference "main"] })
      evalContext ctx input emitParam `shouldBe` expected

  describe "emitInstr" $ do
    it "emits byte for no-param instruction" $ do
      let ctx = E.empty
      let input = Instruction (Operator Halt) Empty
      let expected = Right (ctx { _beans = [Byte 0x01] })
      evalContext ctx input emitInstr `shouldBe` expected
    it "emits bytes for 2-param instruction" $ do
      let ctx = E.empty
      let input = Instruction (Operator Push) (Params [(Param (Integer 11)), (Param (LabelRef "main"))])
      let expected = Right (ctx { _beans = [Byte 0x02, Byte 0x0B, Reference "main"] })
      evalContext ctx input emitInstr `shouldBe` expected

  describe "emit" $ do
    it "label resolution works" $ do
      let input = "main: \n\
                  \push 1\n\
                  \push 2\n\
                  \jmp &sum\n\
                  \\n\
                  \sum: add\n\
                  \jmp &main"      
      let (Right tokens) = tokenize input
      let (Right ast) = parse tokens
      let expected = [0x02, 0x01, 0x02, 0x02, 0x0e, 0x06, 0x06, 0x0e, 0x00]
      emit ast `shouldBe` Right expected
    it "raises error if label has not been defined" $ do
      let input = "main: \n\
                  \push 1\n\
                  \push 2\n\
                  \jmp &sum\n\
                  \\n\
                  \sum: add\n\
                  \jmp &program"      
      let (Right tokens) = tokenize input
      let (Right ast) = parse tokens      
      emit ast `shouldBe` Left "Label 'program' is not defined"