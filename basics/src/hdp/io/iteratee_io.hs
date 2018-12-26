module Iteratee_IO where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Data.Char             (chr)
import           Iteratee_Types
import           System.IO

main = do
  h <- openFile "x.txt" ReadMode

  chunk1 <- B.hGet h 25
  let (NeedChunk iter1) = runIter chunkIter chunk1

  chunk2 <- B.hGet h 25
  let (HaveLine line residual) = runIter iter1 chunk2
  putStrLn line
