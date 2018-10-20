module NiceFork where

import           Control.Concurrent
import           Control.Exception  (Exception, try)
import qualified Data.Map           as M

data ThreadException = KilledByUncaughtException deriving (Eq,Show)
instance Exception ThreadException

data ThreadStatus = Running
                  | Finished
                  | Threw ThreadException
                    deriving (Show, Eq)

newtype ThreadManager = Mgr (MVar (M.Map ThreadId (MVar ThreadStatus))) deriving Eq

-- | Create a new thread manager
newManager :: IO ThreadManager
newManager = fmap Mgr (newMVar M.empty)

-- | Create a new managed thread
forkManaged :: ThreadManager -> IO () -> IO ThreadId
forkManaged (Mgr mvar) body =
  modifyMVar mvar $ \tidMap -> do
  state <- newEmptyMVar
  tid <- forkIO $ do
    result <- try body
    putMVar state (either Threw (const Finished) result)
  return (M.insert tid state tidMap, tid)


-- | Immediately return the status of a managed Thread
getStatus :: ThreadManager -> ThreadId -> IO (Maybe ThreadStatus)
getStatus (Mgr mVar) tid =
  modifyMVar mVar $ \m ->
                      case M.lookup tid m of
                        Nothing -> return (m, Nothing)
                        Just st -> tryTakeMVar st >>= \mst -> case mst of
                                                                Nothing -> return (m, Just Running)
                                                                Just sth -> return (M.delete tid m, Just sth)

-- | Block until a specified managed thread terminates
waitFor :: ThreadManager -> ThreadId -> IO(Maybe ThreadStatus)
waitFor (Mgr mVar) tid = do
  maybeDone <- modifyMVar mVar $ \m ->
    return $ case M.updateLookupWithKey (\_ _ -> Nothing) tid m of
               (Nothing,_) -> (m, Nothing)
               (done, m')  -> (m', done)
  case maybeDone of
    Nothing -> return Nothing
    Just st -> Just `fmap` takeMVar st

-- | Block until all managed threads terminate
waitAll :: ThreadManager -> IO ()
waitAll (Mgr mVar) = modifyMVar mVar elems >>= mapM_ takeMVar
  where elems m = return (M.empty, M.elems m)
