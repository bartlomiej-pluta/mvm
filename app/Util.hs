module Util (
  toLowerCase,
  byteStr,
  bytesStr,
  head,
  unescape,
  controlChar,
  explode
) where

import Prelude hiding (head)
import Data.List hiding (head)
import Data.Word
import Numeric (showHex)
import qualified Data.Char as Char

toLowerCase :: String -> String
toLowerCase = map Char.toLower

bytesStr :: Int -> [Word8] -> String
bytesStr sparse = insertAtN '\n' (sparse*3) . intercalate " " . map byteStr

byteStr :: Word8 -> String
byteStr = pad '0' 2 . (flip showHex) "" . (fromIntegral :: Word8 -> Integer)

insertAtN :: a -> Int -> [a] -> [a]
insertAtN c n xs = insertAtN' n xs
  where
    insertAtN' 0 ys = c : insertAtN' n ys
    insertAtN' _ [] = []
    insertAtN' m (y:ys) = y : insertAtN' (m-1) ys

pad :: Char -> Int -> String -> String
pad char width string = replicate (width - length string) char ++ string

head :: [a] -> Maybe a
head []    = Nothing
head (x:_) = Just x

unescape :: String -> Maybe String
unescape ('\\':x:xs) = do
  cc <- fmap Char.chr $ controlChar x
  rest <- unescape xs
  return $ cc : rest
unescape (x:xs) = unescape xs >>= (\rest -> return $ x : rest)
unescape [] = Just []

controlChar :: Char -> Maybe Int
controlChar x = case x of
    'n'  -> Just 10
    't'  -> Just 9
    'v'  -> Just 11
    'b'  -> Just 8
    'r'  -> Just 13
    'f'  -> Just 12
    'a'  -> Just 7
    '\\' -> Just 92
    '"'  -> Just 34
    '\'' -> Just 39
    '0'  -> Just 0
    _    -> Nothing

explode :: (Foldable f) => (a -> Bool) -> f a -> [[a]]
explode predicate xs = filter (not . null) $ foldr split [[]] xs 
  where 
    split y (ys:yss) 
      | predicate y = []:ys:yss
      | otherwise   = (y:ys):yss