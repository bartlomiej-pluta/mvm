module Parser (
  parse
) where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Map as Map

import qualified Instruction as I
import qualified Util as U

parse :: B.ByteString -> Either String [I.Command]
parse = parseCommands . B.unpack

parseCommands :: [Word8] -> Either String [I.Command]
parseCommands [] = Right []
parseCommands code@(x:_) = case parseCommand code of
  Just (cmd, rest) -> parseCommands rest >>= (\r -> return $ cmd : r)                              
  Nothing          -> Left $ "Unparseable byte: " ++ U.byteStr x ++ "\nIn following sequence:\n" ++ U.bytesStr 16 code

parseCommand :: [Word8] -> Maybe (I.Command, [Word8])
parseCommand [] = Nothing
parseCommand (opByte:xs) = do
  let op = toEnum . fromIntegral $ opByte :: I.Op
  instruction <- Map.lookup op I.instructionByOp
  let noParams = I.noParams instruction
  let params = map fromIntegral $ take noParams xs :: [Int]
  return (I.Command instruction params, drop noParams xs)