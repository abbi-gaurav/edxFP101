module Lazy_IO where

import           Control.Monad
import           System.IO

lineStream h = hGetContents h >>= return . lines

sequence_ :: [IO()] -> IO ()
sequence_ = foldr (>>) (return ())

--forM_ :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ()
--mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
main = do
  withFile "x.txt" ReadMode enumerateLines
  where
    enumerateLines h = lines' h >>= mapM_ putStrLn
    lines' h' = hGetContents h' >>= return . lines
