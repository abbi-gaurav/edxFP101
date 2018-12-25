module IO_Patterns where

import           Control.Applicative
import           Control.Monad
import           System.IO

main :: IO ()
main = do
  h <- openFile "x.txt" ReadMode
  line <- hGetLine h
  putStrLn . show . words $ line
  hClose h
