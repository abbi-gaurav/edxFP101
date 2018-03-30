module SimpleFinder where
import           RecursiveContents

simpleFind :: (FilePath -> Bool) -> FilePath -> IO[FilePath]
simpleFind predicate path = do
  names <- getRecursiveContents path
  return (filter predicate names)
