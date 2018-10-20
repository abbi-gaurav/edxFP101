module ModifyMVar where

import           Control.Concurrent (MVar, putMVar, takeMVar)
import           Control.Exception  (SomeException, catch, mask, throw)
import           Prelude            hiding (catch)


modifyMVar :: MVar a -> (a -> IO (a,b)) -> IO b
modifyMVar m io =
  mask $ \restore -> do
  a <- takeMVar m
  (a',b) <- restore (io a) `catch` \e ->
                                     putMVar m a >> throw (e :: SomeException)
  putMVar m a'
  return b
