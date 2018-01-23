{-# LANGUAGE FlexibleInstances #-}

module JSONClass where

import           Control.Arrow (second)
import           JSONModel

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
  fromJValue _         = Left "Not a boolean value"

instance JSON String where
  toJValue = JString

  fromJValue (JString str) = Right str
  fromJValue _             = Left "Not a string value"

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

jaryFromJValue :: (JSON a) => JValue -> Either JSONError (JAry a)
jaryFromJValue (JArray (JAry a)) = whenRight JAry (mapEithers fromJValue a)
jaryFromJValue _                 = Left "not a json array"

whenRight :: (b -> c) -> Either a b -> Either a c
whenRight _ (Left err) = Left err
whenRight f (Right a)  = Right (f a)

mapEithers :: (a -> Either b c) -> [a] -> Either b [c]
mapEithers f (x : xs) = case mapEithers f xs of
                          Left err -> Left err
                          Right ys -> case f x of
                                        Left err -> Left err
                                        Right y  -> Right (y : ys)
mapEithers _ _ = Right []

jaryToJValue :: (JSON a) => (JAry a) -> JValue
jaryToJValue = JArray .  JAry  . map toJValue  . fromJAry

instance (JSON a) => JSON (JAry a) where
  toJValue = jaryToJValue
  fromJValue = jaryFromJValue

listToJValues :: (JSON a) => [a] -> [JValue]
listToJValues = map toJValue

jvaluesToJAry :: [JValue] -> JAry JValue
jvaluesToJAry = jary

jaryOfJValuesToJValue :: JAry JValue -> JValue
jaryOfJValuesToJValue = JArray

instance (JSON a) => JSON (JObj a) where
  toJValue = JObject. JObj . map (second toJValue) . fromJObj
  fromJValue (JObject (JObj o)) = whenRight JObj (mapEithers unwrap o)
    where unwrap (k,v) = whenRight ((,) k) (fromJValue v)
  fromJValue _                  = Left "not a json object"
