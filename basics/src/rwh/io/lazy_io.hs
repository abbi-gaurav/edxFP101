module Lazy_IO where

import           Control.Monad
import           System.IO

lineStream h = hGetContents h >>= return . lines

sequence_ :: [IO()] -> IO ()
sequence_ = foldr (>>) (return ())

main = do
  h <- openFile "x.txt" ReadMode
  lines' <- lineStream h
  mapM_ putStrLn lines' --sequence_ (map putStrLn lines')
  hClose h
