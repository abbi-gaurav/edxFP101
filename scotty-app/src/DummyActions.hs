{-# LANGUAGE OverloadedStrings #-}

module DummyActions where

import           Data.Monoid ((<>))
import           Web.Scotty  (ActionM, html, param, text)

wordHandler :: ActionM()
wordHandler = do
  beam <- param "word"
  html $ mconcat ["<h1>ScottY, ", beam, " me up!</h1>"]

hello :: ActionM()
hello = do
  name <- param "name"
  text ("hello " <> name <> "!")
