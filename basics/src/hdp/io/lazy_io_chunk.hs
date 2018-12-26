module Lazy_IO_Chunk where

import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Char                  (chr)
import           System.IO

data Chunk = Chunk {chunk :: String}
           | LineEnd {chunk :: String, remainder :: String}
           deriving (Show)

parseChunk :: LB.ByteString -> Chunk
parseChunk str =
  if rightS == L8.pack ""
  then Chunk   (toS leftS)
  else LineEnd (toS leftS) ((toS . L8.tail) rightS)
  where
    (leftS,rightS) = L8.break ( == '\n') str

    toS :: L8.ByteString -> [Char]
    toS = map (chr . fromEnum) . LB.unpack

--producer
chunkStream :: Handle -> IO [L8.ByteString]
chunkStream h = do
  isEof <- hIsEOF h
  if isEof
    then return []
    else
    do
      chunk <- LB.hGet h 8
      rest <- (chunkStream h)
      return (chunk : rest)

--consumer
processChunk' :: String -> [L8.ByteString] -> IO ()
processChunk' acc [] = do putStrLn acc
processChunk' acc (chunk:chunks) =
  case (parseChunk chunk) of
    (Chunk chunk')
      -> do
      processChunk' (acc ++ chunk') chunks
    (LineEnd chunk' rest)
      -> do
      let line = acc ++ chunk'
      putStrLn line
      processChunk' rest chunks

processChunk :: [L8.ByteString] -> IO ()
processChunk = processChunk' ""

main = do
  h      <- openFile "x.txt" ReadMode
  chunks <- chunkStream h
  processChunk chunks
  hClose h
