module BetterPredicate where

import           Control.Exception      (bracket, handle)
import           Control.Monad          (filterM)
import           Data.Time.Clock.System (SystemTime (..))
import           RecursiveContents
import           RecursiveContents      (getRecursiveContents)
import           System.Directory       (Permissions (..), getModificationTime,
                                         getPermissions)
import           System.FilePath        (takeExtension)
import           System.IO              (IOMode (..), hClose, hFileSize,
                                         openFile)

type Predicate = FilePath -> Permissions -> Maybe Integer -> SystemTime -> Bool
