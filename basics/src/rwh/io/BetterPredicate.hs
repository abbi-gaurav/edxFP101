module BetterPredicate where

import           Control.Exception (SomeException, bracket, handle)
import           Control.Monad     (filterM)
import           Data.Time.Clock
import           RecursiveContents
import           RecursiveContents (getRecursiveContents)
import           System.Directory  (Permissions (..), getModificationTime,
                                    getPermissions)
import           System.FilePath   (takeExtension)
import           System.IO         (IOMode (..), hClose, hFileSize, openFile)

type Predicate = FilePath             -- path to the directory entry
                 -> Permissions
                 -> Maybe Integer     -- file size (nothing if not a file)
                 -> UTCTime           -- last modified
                 -> Bool

getFileSize :: FilePath -> IO(Maybe Integer)
getFileSize path = handle errorHandler $
  bracket (openFile path ReadMode) hClose $ \h -> do
  size <- hFileSize h
  return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

simpleFileSize :: FilePath -> IO Integer
simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle errorHandler $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)


errorHandler :: SomeException -> IO (Maybe Integer)
errorHandler _ = return Nothing
