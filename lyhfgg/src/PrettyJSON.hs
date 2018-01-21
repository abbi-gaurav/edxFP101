module PrettyJSON where

import           Prettify
import           JSONModel

renderJValue :: JValue -> Doc
renderJValue (JString s)    = text s
renderJValue (JNumber n)    = double n
renderJValue (JBool True)   = text "true"
renderJValue (JBool False)  = text "false"
renderJValue (JNull)        = text "null"
renderJValue (JArray array) = series '[' ']' renderJValue array
renderJValue (JObject obj)  = series '{' '}' field obj
  where field (name, val) = string name
                            <> text ": "
                            <> renderJValue val
