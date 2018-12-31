{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Simple_Arrow where

import           Control.Arrow
import           Control.Category
import           Control.Monad.Identity
import           Prelude                hiding (id, (.))

data IOArrow a b = IOArrow { runIOArrow :: a -> IO b }

instance Category IOArrow where
  id :: IOArrow a a
  id = IOArrow return

  -- composition
  (.) :: IOArrow b c -> IOArrow a b -> IOArrow a c
  IOArrow f . IOArrow g = IOArrow $ f <=< g

instance Arrow IOArrow where
  -- lift
  arr :: (b -> c) -> IOArrow b c
  arr f = IOArrow $ return . f

  first :: IOArrow b c -> IOArrow (b, d) (c, d)
  first (IOArrow f) = IOArrow $ \(a,c) -> do
    x :: c <- f a
    return (x,c)

foo :: Int -> String
foo = show

bar :: String -> IO Int
bar = return . read

main :: IO()
main = do
  let f  = arr (++ "!") . arr foo . IOArrow bar . arr id
  result <- runIOArrow f "123"
  putStrLn result

main2 :: IO ()
main2 = do
  let f :: IOArrow FilePath () = IOArrow print      -- IOArrow Int ()
                                 . arr length       -- IOarrow [String] Int
                                 . arr words        -- IOArrow String [String]
                                 . IOArrow readFile --IOArrow Filepath String
  runIOArrow f "x.txt"

useKleisli :: IO ()
useKleisli = do
  let f = Kleisli print
          . arr length
          . arr words
          . Kleisli readFile
  runKleisli f "x.txt"

firstAndSecond :: IO ()
firstAndSecond = do
  let f = IOArrow print
          . (second (arr $ fmap head)) -- only modify second of tuple
          . (first (arr length))       -- only modify first of tuple
          . arr (\x -> (x,x))
          . arr words
          . IOArrow readFile
  runIOArrow f "x.txt"

firstAndSecondCombined :: IO ()
firstAndSecondCombined = do
  let f = IOArrow print
          . (arr length *** arr head)
          . arr (\x -> (x,x))
          . arr words
          . IOArrow readFile
  runIOArrow f "x.txt"
