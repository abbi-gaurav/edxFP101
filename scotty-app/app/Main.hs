{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


import           Config
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, runReaderT)
import           Data.Monoid            ((<>))
import           Data.Text.Lazy         (Text)
import           DummyActions
import           UserActions
import           Web.Scotty             (ScottyM, scotty)
import           Web.Scotty.Trans

type Error = Text

main :: IO ()
main = scotty 3000 routes

routes :: ScottyM()
routes = do
  get "/users" $ usersH
  get "/:word" $ wordHandler
  get "/hello/:name" $ hello
