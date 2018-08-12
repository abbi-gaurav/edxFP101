module FormApp where

import           Control.Applicative                hiding (many, (<|>))
import           Numeric
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Char

a_char :: CharParser () Char
a_char = oneOf urlBaseChars
  <|> (' ' <$ char '+')
  <|> a_hex

urlBaseChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "$-_.!*'(),"

a_hex :: CharParser () Char
a_hex = hexify <$> (char '%' *> hexDigit) <*> hexDigit
  where hexify a b = toEnum . fst . head . readHex $ [a,b]

a_pair :: CharParser () (String, Maybe String)
a_pair = liftA2 (,) (many1 a_char) (optionMaybe (char '=' *> many a_char))

a_query :: CharParser () [(String, Maybe String)]
a_query = a_pair `sepBy` char '&'
