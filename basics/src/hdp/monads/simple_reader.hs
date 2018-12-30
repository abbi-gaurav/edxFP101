module Simple_Reader where

import           Control.Monad.Reader

data Config = Config { starter :: String}

tom :: Reader Config String
tom = do
  env <- asks starter
  return (env ++ " This is Tom.")

jerry :: Reader Config String
jerry = do
  env <- ask
  return (starter env ++ " This is jerry")

tomAndJerry :: Reader Config String
tomAndJerry = do
  t <- tom
  j <- jerry
  return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) (Config "who is this")

