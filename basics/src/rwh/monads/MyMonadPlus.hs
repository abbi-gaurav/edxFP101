module MyMonadPlus where

import           Control.Monad

-- MonadPlus
-- mzero >>= f == mzero
-- v >> mzero


guard :: (MonadPlus m) => Bool -> m()
guard True = return ()
guard _    = mzero

zeroMod :: (MonadPlus m, Integral b) => b -> b -> m b
x `zeroMod` n = MyMonadPlus.guard((x `mod` n) == 0) >> return x
