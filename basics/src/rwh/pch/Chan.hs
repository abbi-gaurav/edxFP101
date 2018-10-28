module Chan where

import           Control.Concurrent
import           Control.Concurrent.Chan

chanExample :: IO ()
chanExample = do
  ch <- newChan
  forkIO $ do
    writeChan ch "hello"
    writeChan ch "quit"
  readChan ch >>= print
  readChan ch >>= print
