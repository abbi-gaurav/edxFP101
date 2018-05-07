module BasicsTest where

import qualified QC          as DocTests
import           System.Exit

main :: IO ()
main = do
  good <- and <$> (sequence [DocTests.runTests])
  if good
    then exitSuccess
    else exitFailure
