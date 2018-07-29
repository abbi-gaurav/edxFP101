{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


import           Data.Monoid  ((<>))
import           DummyActions
import           UserActions
import           Web.Scotty

main :: IO ()
main = scotty 3000 routes

routes :: ScottyM()
routes = do
  get "/users" $ usersH
  get "/:word" $ wordHandler
  get "/hello/:name" $ hello
