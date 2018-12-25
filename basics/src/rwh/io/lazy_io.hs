module Lazy_IO where

import           Control.Monad
import           System.IO

main = do
  h <- openFile "x.txt" ReadMode
  contents <- hGetContents h
  putStrLn (take 10 contents)
  hClose h
