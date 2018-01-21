{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}


module JSONClass where

import JSONModel

type JSONError = String

class JSON a where
  toJValue :: a -> JValue
  fromJValue :: JValue -> Either JSONError a

instance JSON JValue where
  toJValue = id
  fromJValue = Right

instance JSON Bool where
  toJValue = JBool

  fromJValue (JBool b) = Right b
  fromJValue _ = Left "Not a boolean value"

instance JSON String where
  toJValue = JString

  fromJValue (JString str) = Right str
  fromJValue _ = Left "Not a string value"

jValueToDouble :: (Double -> a) -> JValue -> Either JSONError a
jValueToDouble f (JNumber v) = Right (f v)
jValueToDouble f _           = Left "Not a json number"

instance JSON Int where
  toJValue    = JNumber . realToFrac
  fromJValue  = jValueToDouble round

instance JSON Integer where
  toJValue        = JNumber . realToFrac
  fromJValue      = jValueToDouble round

instance JSON Double where
  toJValue        = JNumber
  fromJValue      = jValueToDouble id

instance (JSON a) => JSON [a] where
  toJValue     = undefined
  fromJValue   = undefined

instance (JSON a) => JSON [(String, a)] where
  toJValue     = undefined
  fromJValue   = undefined
