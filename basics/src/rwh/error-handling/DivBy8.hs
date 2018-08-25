{-# LANGUAGE FlexibleContexts #-}
module DivBy8 where

data DivByError a = DivBy0
                  | ForbiddenDenominator a
                  | OtherDivByError String
                  deriving (Show, Eq, Read)
