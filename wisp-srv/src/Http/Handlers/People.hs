-- src/Http/Handlers/People.hs
module Http.Handlers.People
  ( getPeople
  , getPersonById
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.HTTP.Types.Status (status404)
import Web.Scotty.Trans (ActionT, json, status, pathParam)
import App.Monad (Env)
import Domain.Id (EntityId(..))
import qualified Infra.Db.Person as Db

-- GET /people - List important people (linked to activities or have relationship)
getPeople :: ActionT (ReaderT Env IO) ()
getPeople = do
  people <- lift Db.getImportantPeople
  json $ object
    [ "people" .= people
    , "count" .= length people
    ]

-- GET /people/:id - Get a specific person
getPersonById :: ActionT (ReaderT Env IO) ()
getPersonById = do
  pid <- pathParam "id"
  mperson <- lift $ Db.getPersonById (EntityId pid)
  case mperson of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Person not found" :: Text)]
    Just person -> json person
