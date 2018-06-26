{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module MonadHandleIO where

import           MonadHandle
import qualified System.IO

import           Control.Monad.Trans (MonadIO (..), MonadTrans (..))
import           System.Directory    (removeFile)
import           System.IO           (IOMode (..))

import           SafeHello


instance MonadHandle System.IO.Handle IO where
  openFile     = System.IO.openFile
  hPutStr      = System.IO.hPutStr
  hClose       = System.IO.hClose
  hGetContents = System.IO.hGetContents

