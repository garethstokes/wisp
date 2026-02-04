module Infra.Db.Person
  ( upsertPerson
  , getPersonByEmail
  , getPersonById
  , getAllPeople
  , getImportantPeople
  , updatePersonContact
  , searchPeople
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.Types (PGArray(..))
import Domain.Id (EntityId(..), newEntityId)
import Domain.Person (Person(..))
import App.Monad (App, getConn)

instance FromRow Person where
  fromRow = Person
    <$> (EntityId <$> field)          -- id
    <*> field                          -- email
    <*> field                          -- display_name
    <*> (fmap fromPGArray <$> field)  -- personas (nullable array)
    <*> field                          -- relationship
    <*> field                          -- organisation
    <*> field                          -- notes
    <*> field                          -- first_contact
    <*> field                          -- last_contact
    <*> field                          -- contact_count
    <*> field                          -- created_at

-- Upsert person by email (returns existing or creates new)
upsertPerson :: Text -> Maybe Text -> App Person
upsertPerson email displayName = do
  conn <- getConn
  pid <- liftIO newEntityId
  _ <- liftIO $ execute conn
    "insert into people (id, email, display_name, first_contact) \
    \values (?, ?, ?, now()) \
    \on conflict (email) do update set \
    \  display_name = coalesce(excluded.display_name, people.display_name)"
    (unEntityId pid, email, displayName)
  -- Fetch the person (might be existing or newly created)
  results <- liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people where email = ?"
    (Only email)
  case results of
    [p] -> pure p
    _ -> error $ "Failed to upsert person: " <> show email

-- Get person by email
getPersonByEmail :: Text -> App (Maybe Person)
getPersonByEmail email = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people where email = ?"
    (Only email)
  pure $ case results of
    [p] -> Just p
    _ -> Nothing

-- Get person by ID
getPersonById :: EntityId -> App (Maybe Person)
getPersonById pid = do
  conn <- getConn
  results <- liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people where id = ?"
    (Only $ unEntityId pid)
  pure $ case results of
    [p] -> Just p
    _ -> Nothing

-- Get all people
getAllPeople :: App [Person]
getAllPeople = do
  conn <- getConn
  liftIO $ query_ conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people order by last_contact desc nulls last"

-- Get important people (linked to activities OR have relationship set)
getImportantPeople :: App [Person]
getImportantPeople = do
  conn <- getConn
  liftIO $ query_ conn
    "select distinct p.id, p.email, p.display_name, p.personas, p.relationship, \
    \p.organisation, p.notes, p.first_contact, p.last_contact, p.contact_count, p.created_at \
    \from people p \
    \where p.relationship is not null \
    \   or exists (select 1 from activities a where a.person_id = p.id) \
    \order by p.last_contact desc nulls last"

-- Update last_contact and increment contact_count
updatePersonContact :: EntityId -> App ()
updatePersonContact pid = do
  conn <- getConn
  _ <- liftIO $ execute conn
    "update people set last_contact = now(), contact_count = contact_count + 1, \
    \updated_at = now() where id = ?"
    (Only $ unEntityId pid)
  pure ()

-- Search people by email or display name
searchPeople :: Text -> Int -> App [Person]
searchPeople searchTerm limit = do
  conn <- getConn
  let pattern = "%" <> searchTerm <> "%"
  liftIO $ query conn
    "select id, email, display_name, personas, relationship, organisation, \
    \notes, first_contact, last_contact, contact_count, created_at \
    \from people \
    \where email ilike ? or display_name ilike ? \
    \order by contact_count desc, last_contact desc nulls last \
    \limit ?"
    (pattern, pattern, limit)
