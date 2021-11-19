module Util (
  toLowerCase,
  byteStr,
  bytesStr,
  head,
  unescape,
  controlChar,
  explode,
  maybeToExcept,
  maybeToEither
) where

import Prelude hiding (head)
import Data.Word (Word8)
import Data.Char (chr, toLower)
import Numeric (showHex)
import Control.Monad.Except (MonadError (throwError))


toLowerCase :: String -> String
toLowerCase = map toLower

bytesStr :: Int -> [Word8] -> String
bytesStr sparse = insertAtN '\n' (sparse*3) . unwords . map byteStr

byteStr :: Word8 -> String
byteStr = pad '0' 2 . flip showHex "" . (fromIntegral :: Word8 -> Integer)

insertAtN :: a -> Int -> [a] -> [a]
insertAtN c n xs = insertAtN' n xs
  where insertAtN' 0 ys = c : insertAtN' n ys
        insertAtN' _ [] = []
        insertAtN' m (y:ys) = y : insertAtN' (m-1) ys

pad :: Char -> Int -> String -> String
pad char width string = replicate (width - length string) char ++ string

head :: [a] -> Maybe a
head []    = Nothing
head (x:_) = Just x

unescape :: String -> Maybe String
unescape ('\\':x:xs) = (:) . chr <$> controlChar x <*> unescape xs
unescape (x:xs) = (x:) <$> unescape xs
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
  where split _ [] = []
        split y (ys:yss)
          | predicate y = []:ys:yss
          | otherwise   = (y:ys):yss

maybeToEither :: Maybe a -> e -> Either e a
maybeToEither m err = case m of
  (Just x) -> Right x
  Nothing  -> Left err

maybeToExcept :: MonadError e m => Maybe a -> e -> m a
maybeToExcept m err = case m of
  (Just x) -> return x
  Nothing  -> throwError err