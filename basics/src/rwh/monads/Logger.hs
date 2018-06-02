module Logger
  (
    Logger
  , Log
  , runLogger
  , record
  ) where

import           Control.Monad (ap, liftM)

type Log = [String]

newtype Logger a = Logger { execLogger :: (a, Log)} deriving Show

runLogger :: Logger a -> (a, Log)
runLogger = execLogger

record :: String -> Logger()
record s = Logger ((),[s])

instance Functor Logger where
  fmap = liftM

instance Applicative Logger where
  pure = return
  (<*>) = ap

instance Monad Logger where
  return a = Logger (a, [])

  -- (>>=) :: Logger a -> (a -> Logger b) -> Logger b
  loggerA >>= f = let (a, logA)  = execLogger loggerA
                      loggerB    = f a
                      (b, logB)  = execLogger loggerB
            in Logger (b, logA ++ logB)
