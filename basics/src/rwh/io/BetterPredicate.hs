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


type Predicate = InfoP Bool

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

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _     = -1

equalP :: (Eq a) => InfoP a  -> a -> InfoP Bool
equalP f k = \w x y z -> f w x y z == k

equalP' :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP' f k w x y z = f w x y z == k

liftP :: (a -> b ->c) -> InfoP a -> b -> InfoP c
liftP q f b' = \w x y z ->
                let a' = f w x y z
                in a' `q` b'

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

simpleAndP :: InfoP Bool -> InfoP Bool -> InfoP Bool
simpleAndP f g = \w x y z -> f w x y z && g w x y z

type InfoP a = FilePath -> Permissions -> Maybe Integer -> UTCTime -> a

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g = \filePath perm maybeSize modTime -> q (f filePath perm maybeSize modTime) (g filePath perm maybeSize modTime)

andP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)

orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
orP = liftP2 (||)

liftPath :: (FilePath -> a) -> InfoP a
liftPath f = \w _ _ _ -> f w

myTest :: InfoP Bool
myTest = let checkExt = (liftPath takeExtension) `equalP` ".cpp"
             checkSize = sizeP `greaterP` 131072
         in checkExt `andP` checkSize

(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP

(||?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(||?) = orP

(==?) :: (Eq a) => InfoP a  -> a -> InfoP Bool
(==?) = equalP

(>?),(<?) :: (Ord a) => InfoP a -> a -> InfoP Bool
(>?) = greaterP
(<?) = lesserP
