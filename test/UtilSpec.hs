module UtilSpec where

import Prelude hiding (head)
import Test.Hspec
import Util

import Data.Word
import Data.Char (chr)

spec :: Spec
spec = do
  describe "toLowerCase" $ do
    it "converts arbitrary string to lowercase" $
      toLowerCase "Some UpPeR CASED sTRi_NG" `shouldBe` "some upper cased stri_ng"
    it "does not convert already lowercased string" $
      toLowerCase "some lower cased string" `shouldBe` "some lower cased string"
  
  describe "byteStr" $ do
    it "presents byte as hex" $ do
      let input    = [0,    1,    5,    10,   15,   16,   255 ] :: [Word8]
      let expected = ["00", "01", "05", "0a", "0f", "10", "ff"]
      map byteStr input `shouldBe` expected

  describe "bytesStr" $ do
    it "presents bytes as hex in fixed columns of width 5" $ do
      let input    = [0..19] :: [Word8]
      let expected = "00 01 02 03 04 \n\
                     \05 06 07 08 09 \n\
                     \0a 0b 0c 0d 0e \n\
                     \0f 10 11 12 13"                
      bytesStr 5 input `shouldBe` expected

  describe "head" $ do
    it "returns just a head of list" $ do
      head [4, 3, 6] `shouldBe` (Just 4 :: Maybe Int)
    it "returns Nothing if list is empty" $ do
      head [] `shouldBe` (Nothing :: Maybe Int)

  describe "unescape" $ do
    it "properly converts escape sequences to correct ASCII characters" $ do
      let input = "\\tHello,\\nworld!\\0"
      let expected = "\tHello,\nworld!\0"
      unescape input `shouldBe` Just expected
    it "supports all escape sequences" $ do
      let input = "\\n\\t\\v\\b\\r\\f\\a\\\\\\\"\\0"
      let expected = map chr [10, 9, 11, 8, 13, 12, 7, 92, 34, 0]
      unescape input `shouldBe` Just expected
    it "returns nothing if unknown escape sequence encountered" $ do
      let input = "Unknown escape: \\x"
      unescape input `shouldBe` Nothing
  
  describe "explode" $ do
    it "splits the list by given character" $ do
      let input = "hello:world: what's : up?"
      let expected = ["hello", "world", " what's ", " up?"]
      explode (==':') input `shouldBe` expected
    it "supports empty input" $ do
      let input = ""      
      explode (==':') input `shouldBe` []
    it "filters out empty lists" $ do
      let input = ":hello:world:::::: what's : up?"
      let expected = ["hello", "world", " what's ", " up?"]
      explode (==':') input `shouldBe` expected