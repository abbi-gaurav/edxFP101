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
--main = runEffect $ for loop (lift . putStrLn)
--main = runEffect $ for (for P.stdinLn triple) (lift . putStrLn)
-- (f ~> g) x = for (f x) g
-- f = for P.stdinln ; x = triple; g = lift.putstrln
main = runEffect $ for P.stdinLn (triple ~> lift.putStrLn)


-- laws
-- Associativity
-- (f ~> g) ~> h = f ~> (g ~> h)

-- s :: Monad m =>      Producer a m ()  -- i.e. 'P.stdinLn'
-- f :: Monad m => a -> Producer b m ()  -- i.e. 'triple'
-- g :: Monad m => b -> Producer c m ()  -- i.e. '(lift . putStrLn)'
--  for (for s f) g = for s (\x -> for (f x) g)
