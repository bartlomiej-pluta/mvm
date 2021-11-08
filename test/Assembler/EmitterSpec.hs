module Assembler.EmitterSpec where

import Test.Hspec

import qualified Data.Map as M


import Assembler.Tokenizer (tokenize)
import Assembler.Parser (parse)
import Assembler.Emitter

spec :: Spec
spec = do
  describe "resolveLabels" $ do
    it "replaces reference with actual byte number" $ do
      let beans = [ Byte 1, Byte 2, Reference "main", Byte 4 ]
      let labels = M.fromList [("main", 3)]
      resolveLabels labels beans `shouldBe` Right [Byte 1, Byte 2, Byte 3, Byte 4]
    it "throws error if label does not exist" $ do
      let beans = [ Byte 1, Byte 2, Reference "not_existing_label", Byte 4 ]
      let labels = M.fromList [("main", 3)]
      resolveLabels labels beans `shouldBe` Left "Label 'not_existing_label' is not defined"

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