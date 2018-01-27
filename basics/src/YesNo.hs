{-# OPTIONS_GHC -fwarn-missing-signatures #-}

module YesNo where

class YesNo a where
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

yesnoIf :: YesNo a => a -> t -> t -> t
yesnoIf yesNoVal yesResult noResult =
    if yesno yesNoVal
        then yesResult
        else noResult
