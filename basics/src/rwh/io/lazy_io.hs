module Lazy_IO where

import           Control.Monad
import           System.IO

lineStream h = hGetContents h >>= return . lines

sequence_ :: [IO()] -> IO ()
sequence_ = foldr (>>) (return ())

--forM_ :: (Monad m, Foldable t) => t a -> (a -> m b) -> m ()
--mapM_ :: (Monad m, Foldable t) => (a -> m b) -> t a -> m ()
main = do
  h <- openFile "x.txt" ReadMode
  lines' <- lineStream h
  forM_ lines' $ \line -> do
    let reversed = reverse line
    putStrLn reversed
  hClose h
