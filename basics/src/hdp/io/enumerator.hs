module Enumerator where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import           Iteratee_Types
import           Iteratee_Utils
import           System.IO

type Enumerator = Iter -> IO IterResult

enumerateFile :: FilePath -> Enumerator
enumerateFile path initIter =
  withFile path ReadMode $ \h ->
  let
    go :: Iter -> IO IterResult
    go iter = do
      isEof <- hIsEOF h
      if isEof
        then return (HaveLine "EOF" "")
        else do
        chunk <- B.hGet h 8
        check $ runIter iter chunk

    check :: IterResult -> IO IterResult
    check (NeedChunk itrNext)      = go itrNext
    check (HaveLine line residual) = do
      putStrLn line
      check $ runIter initIter (B8.pack residual)
  in go initIter

main = do enumerateFile "x.txt" chunkIter
