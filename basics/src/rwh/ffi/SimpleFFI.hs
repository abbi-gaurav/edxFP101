{-# LANGUAGE ForeignFunctionInterface #-}

module SimpleFFI where

import           Foreign
import           Foreign.C.Types


foreign import ccall "math.h sin"
    c_sin :: CDouble -> CDouble

fastSin :: Double -> Double
fastSin x = realToFrac(c_sin (realToFrac x))

main = mapM_ (print . fastSin) [0/10, 1/10 .. 10/10]
