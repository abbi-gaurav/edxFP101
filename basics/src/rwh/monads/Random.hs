module Random where

import           Control.Monad.State
import           System.Random

type RandomState a = State StdGen a

getRandom :: Random a => RandomState a
getRandom =
  get >>= \gen ->
  let (val, gen') = random gen
  in put gen' >>
  return val

getRandom2 :: Random a => RandomState a
getRandom2 = do
  gen <- get
  let (val, gen') = random gen
  put gen'
  return val

getTwoRandoms :: Random a => RandomState (a,a)
getTwoRandoms = liftM2 (,) getRandom2 getRandom2

runTwoRandoms :: IO (Int, Int)
runTwoRandoms = do
  oldState <- getStdGen
  let (result, newState) = runState getTwoRandoms oldState
  setStdGen newState
  return result

data CountedRandom = CountedRandom {
    crGen   :: StdGen
  , crCount :: Int
  }

type CRState a = State CountedRandom a

getCountedRandom :: Random a => CRState a
getCountedRandom = do
  crState <- get
  let (val, gen') = random (crGen crState)
  put CountedRandom { crGen = gen', crCount = crCount crState + 1 }
  return val

getCount :: CRState Int
getCount = crCount `liftM` get

putCount :: Int -> CRState ()
putCount count = do
  crState <- get
  put crState { crCount = count }

putCountModify :: Int -> CRState ()
putCountModify count = modify $ \crState -> crState { crCount = count }
