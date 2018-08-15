module HttpRequestParser where

import           Control.Applicative           (liftA2)
import           Control.Monad                 (liftM4)
import           Numeric                       (readHex)
import           System.IO                     (Handle)
import           Text.ParserCombinators.Parsec

data Method = Get | Post
            deriving (Eq, Ord, Show)

data HttpRequest = HttpRequest {
    reqMethod  :: Method
  , reqURL     :: String
  , reqHeaders :: [(String, String)]
  , reqBody    :: Maybe String
  } deriving (Eq, Show)

p_request :: CharParser () HttpRequest
p_request = q "GET" Get (pure Nothing)
            <|> q "POST" Post (Just <$> many anyChar)
  where
    q :: String -> Method -> CharParser () (Maybe String) -> CharParser () HttpRequest
    q name ctor body = liftM4 HttpRequest req url p_headers body
          where
            req :: CharParser () Method
            req = ctor <$ string name <* char ' '
    url :: CharParser () String
    url = optional (char '/') *>
          manyTill notEOL (try $ string " HTTP/1." <* oneOf "01")
          <* crlf

p_headers :: CharParser st [(String, String)]
p_headers = header `manyTill` crlf
  where
    header :: CharParser st (String, String)
    header = liftA2 (,) fieldName (char ':' *> spaces *> contents)

    contents :: CharParser st String
    contents = liftA2 (++) (many1 notEOL <* crlf)
                           (continuation <|> pure [])
    continuation :: CharParser st String
    continuation = liftA2 (:) (' ' <$ many1 (oneOf " \t")) contents

    fieldName :: CharParser st String
    fieldName = (:) <$> letter <*> many fieldChar

    fieldChar :: CharParser st Char
    fieldChar = letter <|> digit <|> oneOf "- "

crlf :: CharParser st ()
crlf = (() <$ string "\r\n") <|> (() <$ newline)

notEOL :: CharParser st Char
notEOL = noneOf "\r\n"
