module Iteratee_IO where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (chr)
import           System.IO

data Chunk = Chunk{ chunk :: String}
           | LineEnd { chunk :: String, remainder :: String}
           deriving Show

toS :: B.ByteString -> String
toS = map (chr . fromEnum) . B.unpack

parseChunk :: B.ByteString -> Chunk
parseChunk bs = if rightS == B8.pack ""
                then Chunk (toS leftS)
                else LineEnd (toS leftS) ((toS . B8.tail) rightS)
  where
    (leftS, rightS) = B8.break ( == '\n') bs

newtype Iter = Iter{runIter :: B8.ByteString -> IterResult}

data IterResult = HaveLine {line :: String, residual :: String}
                | NeedChunk Iter

instance Show IterResult where
  show (HaveLine l r) = "Haveline " ++ l ++ " | " ++ r
  show (NeedChunk _)  = "Need chunk"

chunkIter :: Iter
chunkIter = Iter (go "")
  where
    go :: String -> B8.ByteString -> IterResult
    go acc chunk =
      case (parseChunk chunk) of
        (Chunk chunk')            -> NeedChunk (Iter (go (acc ++ chunk')))
        (LineEnd chunk' residual) -> HaveLine (acc ++ chunk') residual

main = do
  h <- openFile "x.txt" ReadMode
  chunk1 <- B.hGet h 50
  print $ runIter chunkIter chunk1
