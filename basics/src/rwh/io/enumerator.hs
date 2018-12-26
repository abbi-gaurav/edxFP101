module Enumerator where

import           System.IO

enumerateFile path initIter =
  withFile path ReadMode $ \h ->
                             let
                               go iter = do
                                 isEof <- hIsEOF h
                                 if isEof
                                   then return ()
