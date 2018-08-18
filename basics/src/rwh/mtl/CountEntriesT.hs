module CountEntriesT where

import           Control.Monad        (forM_, when)
import           Control.Monad.Trans  (liftIO)
import           Control.Monad.Writer (WriterT, tell)
import           CountEntries         (listDirectory)
import           System.Directory     (doesDirectoryExist)
import           System.FilePath      ((</>))

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName
