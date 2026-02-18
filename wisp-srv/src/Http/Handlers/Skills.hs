module Http.Handlers.Skills
  ( getSkillsList
  , getSkillByName
  , putSkillPrompt
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON, ToJSON, Value, object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400, status404)
import Web.Scotty.Trans (ActionT, json, status, jsonData, captureParam)
import App.Monad (Env)
import Skills.Registry (Skill(..), getSkill, allSkillNames)
import Agents.Core (loadSkillPrompt)
import Domain.Id (EntityId(..))
import Domain.Skill (skillTag, skillPromptTags)
import Infra.Db.Activity (getActivitiesByTags, insertNote, updateActivityTitle)
import Domain.Activity (Activity(..), activityId)

-- | Default account ID for now (TODO: get from auth)
defaultAccount :: EntityId
defaultAccount = EntityId "default"

--------------------------------------------------------------------------------
-- Skills Endpoints
--------------------------------------------------------------------------------

-- | GET /api/skills - list available skills
getSkillsList :: ActionT (ReaderT Env IO) ()
getSkillsList = do
  let skills = map getSkillInfo allSkillNames
  json $ object
    [ "skills" .= skills
    , "count" .= length skills
    ]

-- | Get skill info for a skill name
getSkillInfo :: Text -> Value
getSkillInfo name = case getSkill name of
  Nothing -> object
    [ "name" .= name
    , "tools" .= ([] :: [Text])
    , "available" .= False
    ]
  Just skill -> object
    [ "name" .= skillName skill
    , "tools" .= skillToolNames skill
    , "available" .= True
    ]

-- | GET /api/skills/:name - get skill details including prompt
getSkillByName :: ActionT (ReaderT Env IO) ()
getSkillByName = do
  name <- captureParam "name"
  case getSkill name of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Skill not found: " <> name :: Text)]
    Just skill -> do
      -- Load skill prompt from knowledge
      mPrompt <- lift $ loadSkillPrompt defaultAccount name
      json $ object
        [ "name" .= skillName skill
        , "tools" .= skillToolNames skill
        , "prompt" .= mPrompt
        , "tag" .= skillTag name
        ]

-- | Request body for updating skill prompt
data UpdateSkillPromptRequest = UpdateSkillPromptRequest
  { skillPrompt :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON UpdateSkillPromptRequest
instance ToJSON UpdateSkillPromptRequest

-- | PUT /api/skills/:name - update skill prompt
putSkillPrompt :: ActionT (ReaderT Env IO) ()
putSkillPrompt = do
  name <- captureParam "name"
  req <- jsonData :: ActionT (ReaderT Env IO) UpdateSkillPromptRequest

  -- Verify skill exists
  case getSkill name of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Skill not found: " <> name :: Text)]
    Just _skill -> do
      -- Find existing skill prompt note or create new one
      let tags = skillPromptTags name
      notes <- lift $ getActivitiesByTags defaultAccount tags 1
      case notes of
        [] -> do
          -- Create new skill prompt note
          mId <- lift $ insertNote defaultAccount (skillPrompt req) tags (object [])
          case mId of
            Just aid -> json $ object
              [ "status" .= ("created" :: Text)
              , "skill" .= name
              , "note_id" .= aid
              ]
            Nothing -> do
              status status400
              json $ object ["error" .= ("Failed to create skill prompt" :: Text)]
        (note:_) -> do
          -- Update existing prompt (stored in title field)
          lift $ updateActivityTitle (activityId note) (skillPrompt req)
          json $ object
            [ "status" .= ("updated" :: Text)
            , "skill" .= name
            , "note_id" .= activityId note
            ]
