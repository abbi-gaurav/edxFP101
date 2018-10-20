module Compressor where

import           Codec.Compression.GZip (compress)
import           Control.Concurrent
import           Control.Exception      (SomeException, handle)
import           Control.Monad          (forever)
import qualified Data.ByteString.Lazy   as L

main :: IO ()
main = do
  putStrLn "Enter a file to compress"
  fileName <- getLine
  case fileName of
    "" -> return ()
    _ -> do
      handle (print :: SomeException -> IO ()) $ do
      content <- L.readFile fileName
      forkIO (compressFile fileName content)
      return ()
      main
    where compressFile path = L.writeFile (path ++ ".gz") . compress
