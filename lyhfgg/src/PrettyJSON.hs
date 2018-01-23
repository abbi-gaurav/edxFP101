module PrettyJSON where

import           JSONClass
import           JSONModel
import           Prettify

renderJValue :: JValue -> Doc
renderJValue (JString s)     = text s
renderJValue (JNumber n)     = double n
renderJValue (JBool True)    = text "true"
renderJValue (JBool False)   = text "false"
renderJValue (JNull)         = text "null"
renderJValue (JArray jary)   = series '[' ']' renderJValue (fromJAry jary)
renderJValue (JObject jObj)  = series '{' '}' field (fromJObj jObj)
  where field (name, val) = string name
                            <> text ": "
                            <> renderJValue val
