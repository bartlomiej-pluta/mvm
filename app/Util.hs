module Util (
  toLowerCase,
  byteStr,
  bytesStr
) where

import Data.List
import Data.Word
import Numeric (showHex)
import qualified Data.Char as Char

toLowerCase :: String -> String
toLowerCase = map Char.toLower

bytesStr :: Int -> [Word8] -> String
bytesStr sparse = insertAtN '\n' (sparse*3) . intercalate " " . map byteStr

byteStr :: Word8 -> String
byteStr = pad '0' 2 . (flip showHex) "" . fromIntegral

insertAtN :: a -> Int -> [a] -> [a]
insertAtN c n xs = insertAtN' n xs
  where
    insertAtN' 0 xs = c : insertAtN' n xs
    insertAtN' _ [] = []
    insertAtN' m (x:xs) = x : insertAtN' (m-1) xs

pad :: Char -> Int -> String -> String
pad char width string = replicate (width - length string) char ++ string