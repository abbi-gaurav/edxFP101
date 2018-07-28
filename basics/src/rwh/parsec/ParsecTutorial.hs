{-# LANGUAGE FlexibleContexts #-}

module ParsecTutorial where

import           Control.Applicative
import           Control.Monad.Identity (Identity)
import           Text.Parsec            ((<?>))
import qualified Text.Parsec            as Parsec

import           Data.Functor.Identity
import           Text.Parsec.Error

parse ::
  Parsec.Stream s Identity t =>
  Parsec.Parsec s () a -> s -> Either ParseError a
parse rule text = Parsec.parse rule "(source)" text

myParser :: Parsec.Parsec String () (String, String)
myParser = do
  letters <- Parsec.many1 Parsec.letter
  Parsec.spaces
  digits <- Parsec.many1 Parsec.digit
  return (letters, digits)


myParser1 :: Parsec.ParsecT String () Identity (String, String)
myParser1 = myParser

mySeperator :: Char -> Parsec.Parsec String () ()
mySeperator char = Parsec.spaces >> Parsec.char char >> Parsec.spaces
{- |
mySeperator c = do
  Parsec.spaces
  Parsec.char c
  Parsec.spaces
-}
myCommaSeparator :: Parsec.Parsec String () ()
myCommaSeparator = mySeperator ','

myPairs :: Parsec.Parsec String () [(String,String)]
{- |
myPairs = Parsec.many $ do
  pair <- myParser
  myCommaSeparator
  return pair
-}
myPairs = Parsec.many $ myParser >>= \pair -> myCommaSeparator >> return pair

myPairs2a :: Parsec.Parsec String () [(String, String)]
myPairs2a = Parsec.endBy myParser myCommaSeparator

myPairs2b :: Parsec.Parsec String () [(String, String)]
myPairs2b = Parsec.sepBy myParser myCommaSeparator

myPairs2 :: Parsec.Parsec String () [(String, String)]
myPairs2 = Parsec.many $  do
  pair <- myParser
  Parsec.choice [Parsec.eof, myCommaSeparator]
  return pair

myPairs3 :: Parsec.Parsec String () [(String, String)]
myPairs3 = Parsec.many $ do
  pair <- myParser
  Parsec.eof <|> myCommaSeparator
  return pair

helloOrHowdy :: Parsec.Parsec String () String
helloOrHowdy  = Parsec.char 'h' >> (Parsec.string "ello" <|> Parsec.string "owdy")

helloOrHowdy' :: Parsec.Parsec String () String
helloOrHowdy' = do
  first <- Parsec.char 'h'
  rest <- Parsec.string "owdy" <|> Parsec.string "ello"
  return (first:rest)

helloOrHowdy2 :: Parsec.Parsec String () String
helloOrHowdy2 = Parsec.try (Parsec.string "hello") <|> Parsec.string "howdy"
