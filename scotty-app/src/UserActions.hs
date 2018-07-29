module UserActions where

import           User
import           Web.Scotty (ActionM, json)

usersH :: ActionM ()
usersH = do
  json allUsers
