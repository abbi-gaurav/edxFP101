module Nested where

import           Pipes
import qualified Pipes.Prelude as P

triple :: Monad m => a -> Producer a m ()
triple x = do
  yield x
  yield x
  yield x

loop :: Producer String IO ()
loop = for P.stdinLn triple

main :: IO ()
main = runEffect $ for loop (lift . putStrLn)

-- s :: Monad m =>      Producer a m ()  -- i.e. 'P.stdinLn'
-- f :: Monad m => a -> Producer b m ()  -- i.e. 'triple'
-- g :: Monad m => b -> Producer c m ()  -- i.e. '(lift . putStrLn)'
--  for (for s f) g = for s (\x -> for (f x) g)
