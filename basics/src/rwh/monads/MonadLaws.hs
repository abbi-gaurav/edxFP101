module MonadLaws where

-- left identity law
-- return x >>= f === f x

-- rigth identity law
-- m >>= return === m

-- associative law
-- m >>= (\x -> f x >>= g) === ( m >>= f ) >>= g
