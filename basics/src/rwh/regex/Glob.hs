module Glob(namesMatching)  where

import           Control.Exception (Exception, SomeException, handle)
import           Control.Monad     (forM)
import           GlobRegex         (matchesGlob)
import           System.Directory  (doesDirectoryExist, doesFileExist,
                                    getCurrentDirectory, getDirectoryContents)
import           System.FilePath   (dropTrailingPathSeparator, splitFileName,
                                    (</>))
isPattern :: String -> Bool
isPattern = any (`elem` "[*?")

namesMatching pat
  | not (isPattern pat) = do
    exists <- doesNameExist pat
    return (if exists then [pat] else [])
  | otherwise = do
      case splitFileName pat of
        ("", baseName) -> do
          currDir <- getCurrentDirectory
          listMatches currDir baseName
        (dirName, baseName) -> do
          dirs <- if isPattern dirName
                     then namesMatching (dropTrailingPathSeparator dirName)
                  else return [dirName]
          let listDir = if isPattern baseName
                then listMatches
                else listPlain
          pathNames <- forM dirs $ \dir -> do
            baseNames <- listDir dir baseName
            return (map (dir </>) baseNames)
          return (concat pathNames)

doesNameExist :: FilePath -> IO Bool
doesNameExist name = do
  fileExists <- doesFileExist name
  if fileExists
    then return True
    else doesDirectoryExist name

listMatches :: FilePath -> String -> IO [String]
listMatches dirName pat = do
    dirName' <- if null dirName
                then getCurrentDirectory
                else return dirName
    handle errorHandler $ do
        names <- getDirectoryContents dirName'
        let names' = if isHidden pat
                     then filter isHidden names
                     else filter (not . isHidden) names
        return (filter (\x -> filterEither(x `matchesGlob` pat)) names')
          where errorHandler :: SomeException -> IO[String]
                errorHandler  = const (return [])

filterEither :: Either a Bool -> Bool
filterEither (Left _)  = False
filterEither (Right b) = b

isHidden :: String -> Bool
isHidden ('.' : _) = True
isHidden _         = False

listPlain :: FilePath -> String -> IO [String]
listPlain dirName baseName = do
  exists <- if null baseName
            then doesDirectoryExist dirName
            else doesNameExist (dirName </> baseName)
  return (if exists then [baseName] else [])
