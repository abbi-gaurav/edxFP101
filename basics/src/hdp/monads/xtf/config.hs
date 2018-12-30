module Config where

import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.Writer

data Config = Config { discountRate  :: Float,
                      currencySystem :: String }

appCfg :: Config
appCfg = Config 10 "R"

discount :: Float -> Reader Config Float
discount amt = do
  discountRate' <- asks discountRate
  return (amt * (1 - discountRate'/100))

display :: Float -> Reader Config String
display amt = do
  currencySym' <- asks currencySystem
  return (currencySym' ++ " " ++ (show amt))

main :: IO ()
main = do
  putStrLn $ runReader doDoubleDiscount appCfg
  where
    doDoubleDiscount :: Reader Config String
    doDoubleDiscount = do 
    firstCut <- discount 100
    second   <- discount firstCut
    display second


