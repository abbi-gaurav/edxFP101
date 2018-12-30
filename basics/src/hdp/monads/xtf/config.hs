{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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


{-|
Reader
------
Writer
-----
-}
newtype App a = App {runApp :: ReaderT Config (Writer String) a}
  deriving (Monad, Applicative, Functor, MonadReader Config, MonadWriter String )

discountWR :: Float -> App Float
discountWR amt = do
  discountRate' <- asks discountRate
  let discounted = amt * (1 - discountRate' / 100)
  tell $ " > Discount " ++ (show amt) ++ " = " ++ (show discounted)
  return discounted

displayWR :: Float -> App String
displayWR amt = do
  currencySym' <- asks currencySystem
  tell " > Displaying..."
  return (currencySym' ++ " " ++ (show amt))

doApp :: App a -> (a, String)
doApp app = runWriter (runReaderT (runApp app) appCfg)

main2 :: IO ()
main2 = do
  print $ doApp doDoubleDiscount
  where
    doDoubleDiscount :: App String
    doDoubleDiscount = discountWR 100 >>= discountWR >>= displayWR
