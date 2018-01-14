module PrettyJSON where

import           Prettify
import           SimpleJson

renderJValue :: JValue -> Doc
renderJValue (JString s)   = text s
renderJValue (JNumber n)   = double n
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue (JNull)       = text "null"
