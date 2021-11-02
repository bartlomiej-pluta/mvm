module Parser (
  parse
) where

import Data.Word
import qualified Data.ByteString as B
import qualified Data.Map as Map

import qualified VirtualMachine as VM
import qualified Util as U

parse :: B.ByteString -> Either String [VM.Command]
parse = parseCommands . B.unpack

parseCommands :: [Word8] -> Either String [VM.Command]
parseCommands [] = Right []
parseCommands code@(x:_) = case parseCommand code of
  Just (cmd, rest) -> parseCommands rest >>= (\r -> return $ cmd : r)                              
  Nothing          -> Left $ "Unparseable byte: " ++ U.byteStr x ++ "\nIn following sequence:\n" ++ U.bytesStr 16 code

parseCommand :: [Word8] -> Maybe (VM.Command, [Word8])
parseCommand [] = Nothing
parseCommand (opByte:xs) = do
  let op = toEnum . fromIntegral $ opByte :: VM.Op
  instruction <- Map.lookup op VM.instructionByOp
  let noParams = VM.noParams instruction
  let params = map fromIntegral $ take noParams xs :: [Int]
  return (VM.Command instruction params, drop noParams xs)