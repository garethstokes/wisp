module Services.PeopleResolver
  ( resolvePersonForActivity
  , extractDisplayName
  , extractEmail
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import App.Monad (App)
import Domain.Activity (Activity(..))
import Domain.Person (Person(..))
import Infra.Db.Person (upsertPerson, updatePersonContact)

-- Resolve or create person for an activity's sender
resolvePersonForActivity :: Activity -> App (Maybe Person)
resolvePersonForActivity activity = case activitySenderEmail activity of
  Nothing -> pure Nothing
  Just senderField -> do
    let email = extractEmail senderField
    let displayName = extractDisplayName senderField
    -- Upsert creates if not exists, updates display_name if we have a better one
    person <- upsertPerson email displayName
    -- Update contact stats
    updatePersonContact (personId person)
    pure $ Just person

-- Extract display name from "Name <email>" format
extractDisplayName :: Text -> Maybe Text
extractDisplayName field =
  let trimmed = T.strip field
  in if "<" `T.isInfixOf` trimmed
     then
       let namePart = T.strip $ T.takeWhile (/= '<') trimmed
           -- Remove surrounding quotes if present
           unquoted = T.dropAround (`elem` ['"', '\'']) namePart
       in if T.null unquoted then Nothing else Just unquoted
     else Nothing

-- Extract email from "Name <email>" or plain "email" format
extractEmail :: Text -> Text
extractEmail field =
  let trimmed = T.strip field
  in if "<" `T.isInfixOf` trimmed
     then
       let afterLt = T.drop 1 $ T.dropWhile (/= '<') trimmed
           email = T.takeWhile (/= '>') afterLt
       in T.strip email
     else trimmed
