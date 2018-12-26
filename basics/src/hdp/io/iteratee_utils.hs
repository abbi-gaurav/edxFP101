module Iteratee_Utils where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (chr)
import           Iteratee_Types

toS :: B.ByteString -> String
toS = map (chr . fromEnum) . B.unpack

parseChunk :: B.ByteString -> Chunk
parseChunk bs = if rightS == B8.pack ""
                then Chunk (toS leftS)
                else LineEnd (toS leftS) ((toS . B8.tail) rightS)
  where
    (leftS, rightS) = B8.break ( == '\n') bs

chunkIter :: Iter
chunkIter = Iter (go "")
  where
    go :: String -> B8.ByteString -> IterResult
    go acc chunk =
      case (parseChunk chunk) of
        (Chunk chunk')            -> NeedChunk (Iter (go (acc ++ chunk')))
        (LineEnd chunk' residual) -> HaveLine (acc ++ chunk') residual
