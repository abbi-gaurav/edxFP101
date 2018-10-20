module MVarExample where

import           Control.Concurrent

communicate :: IO ()
communicate = do
  m <- newEmptyMVar :: IO (MVar String)
  forkIO $ do
    v <- takeMVar m
    putStrLn ("received " ++ show v)
  putStrLn("sending")
  putMVar m "wake up"
