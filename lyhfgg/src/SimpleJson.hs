module SimpleJson where

import           Data.List (intercalate)
import JSONModel

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: Integral a => JValue -> Maybe a
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber d) = Just d
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray arr) = Just arr
getArray _            = Nothing

isNull :: JValue -> Bool
isNull v = v == JNull


renderJValue :: JValue -> String
renderJValue (JString s)   = show s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue (JNull)       = "null"
renderJValue (JObject o)   = "{" ++ pairs o ++ "}"
  where pairs [] = ""
        pairs ps = intercalate "," (map renderPair ps)
        renderPair (k,v) = show k ++ ": " ++ renderJValue v

putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)
