module Try where

import           Control.Monad (ap, liftM)

data Expr = Lit Int | Div Expr Expr

data Try a = Err String | Return a

instance Functor Try where
  fmap = liftM

instance Applicative Try where
  pure  = return
  (<*>) = ap

instance Monad Try where
  return x = Return x

  fail msg = Err msg

  Err e    >>=  _    = Err e
  Return a >>=  f    = f a

divTry :: Int -> Int -> Try Int
divTry a b = if b == 0
             then Err "Div by Zero"
             else Return (a `div` b)

evalTry :: Expr -> Try Int
evalTry (Lit a)   = Return a
evalTry (Div a b) = do
  a' <- (evalTry a)
  b' <- (evalTry b)
  divTry a' b'
