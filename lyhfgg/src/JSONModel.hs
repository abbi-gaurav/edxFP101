module  JSONModel where

newtype JAry a = JAry {
  fromJAry :: [a]
  } deriving (Eq, Ord, Show)

jary :: [a] -> JAry a
jary = JAry

newtype JObj a = JObj {
  fromJObj :: [(String, a)]
  } deriving (Eq, Ord, Show)


data JValue = JString String
            | JNumber Double
            | JBool Bool
            | JNull
            | JObject (JObj JValue)
            | JArray (JAry JValue)
            deriving (Eq, Ord, Show)
