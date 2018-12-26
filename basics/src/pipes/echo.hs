module Echo where

import           Control.Monad (unless)
import           Pipes
import           System.IO     (isEOF)

stdinLn :: Producer String IO ()
stdinLn = do
  eof <- lift isEOF
  unless eof $ do
    str <- lift getLine
    yield str
    stdinLn

loop :: Effect IO ()
--loop = for stdinLn $ \str -> do
--  lift $ putStrLn str
loop = for stdinLn (lift . putStrLn)

main :: IO ()
main = runEffect loop
