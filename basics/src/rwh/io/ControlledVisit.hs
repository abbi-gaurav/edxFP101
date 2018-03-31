module ControlledVisit where

import           Control.Exception
import           Control.Monad
import           Data.Time.Clock
import           RecursiveContents
import           System.Directory
import           System.FilePath
import           System.IO

data Info = Info {
  infoPath      :: FilePath
  , infoPerms   :: Maybe Permissions
  , infoSize    :: Maybe Integer
  , infoModTime :: Maybe UTCTime
  }

maybeIO :: IO a -> IO (Maybe a)
maybeIO action = handle errorHandler (Just `liftM` action)
  where errorHandler = (\_ -> return Nothing) :: SomeException -> IO (Maybe a)

getInfo :: FilePath -> IO Info
getInfo path = do
  perms <- maybeIO (getPermissions path)
  size <- maybeIO (bracket (openFile path ReadMode) hClose hFileSize)
  modified <- maybeIO (getModificationTime path)
  return (Info path perms size modified)


traverseFP :: ([Info] -> [Info]) -> FilePath -> IO [Info]
traverseFP order path = do
  names <- getUsefulContents path
  let fullNames = (path : map (path </>) names)
  contents <- mapM getInfo fullNames
  -- liftM concat $ forM [1] (\x -> Just[x])
  liftM concat $ forM (order contents) recurse
  where recurse info = do
          if isDirectory info && infoPath info /= path
            then traverseFP order (infoPath info)
            else return [info]

isDirectory :: Info -> Bool
isDirectory = maybe False searchable . infoPerms

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
  names <- getDirectoryContents path
  return (filter (`notElem` [".", ".."]) names)
