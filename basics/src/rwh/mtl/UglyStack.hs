{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module UglyStack where

import           Control.Monad.Reader
import           Control.Monad.State
import           System.Directory
import           System.FilePath

data AppConfig = AppConfig {
  maxDepth :: Int
  } deriving (Show)

data AppState = AppState {
  deepestReached :: Int
  } deriving (Show)

newtype MyApp a = MyA {
  runA :: ReaderT AppConfig (StateT AppState IO) a
  }deriving (Applicative, Functor, Monad, MonadIO, MonadReader AppConfig, MonadState AppState)

--type App = ReaderT AppConfig (StateT AppState IO)

--runApp (constrainedCount 0 "src") 4
runApp :: MyApp a -> Int -> IO(a, AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state  = AppState 0
  in runStateT (runReaderT (runA k) config) state

constrainedCount :: Int -> FilePath -> MyApp[(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg <- ask
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO . doesDirectoryExist $ newPath
    if isDir && curDepth < maxDepth cfg
      then do
      let newDepth = curDepth + 1
      st <- get
      when (deepestReached st < newDepth) $
        put st {deepestReached = newDepth}
      constrainedCount newDepth newPath
      else return []
  return $ (path, length contents) : concat rest
