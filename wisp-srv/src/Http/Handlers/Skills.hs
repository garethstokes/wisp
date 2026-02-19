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
import Network.HTTP.Types.Status (status400, status404, status500)
import Web.Scotty.Trans (ActionT, json, status, jsonData, captureParam)
import App.Monad (Env)
import Skills.Registry (Skill(..), getSkill, allSkillNames)
import Agents.Core (loadSkillPromptByTenant)
import Domain.Skill (skillTag, skillPromptTags)
import Domain.Tenant (TenantId, Tenant(..))
import Infra.Db.Activity (getActivitiesByTagsTenant, updateActivityTitle)
import Infra.Db.Tenant (getAllTenants)
import Domain.Activity (Activity(..), activityId)

--------------------------------------------------------------------------------
-- Tenant resolution (TODO: get from auth token)
--------------------------------------------------------------------------------

-- | Get the current tenant (temporary: uses first tenant)
getCurrentTenant :: ActionT (ReaderT Env IO) (Maybe TenantId)
getCurrentTenant = do
  tenants <- lift getAllTenants
  pure $ case tenants of
    (t:_) -> Just (tenantId t)
    [] -> Nothing

-- | Run action with tenant, or return 500 if no tenant
withTenant :: (TenantId -> ActionT (ReaderT Env IO) ()) -> ActionT (ReaderT Env IO) ()
withTenant action = do
  mTenant <- getCurrentTenant
  case mTenant of
    Nothing -> do
      status status500
      json $ object ["error" .= ("No tenant configured. Create one with: wisp tenant create <name>" :: Text)]
    Just tid -> action tid

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
getSkillByName = withTenant $ \tid -> do
  name <- captureParam "name"
  case getSkill name of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Skill not found: " <> name :: Text)]
    Just skill -> do
      -- Load skill prompt from knowledge
      mPrompt <- lift $ loadSkillPromptByTenant tid name
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
putSkillPrompt = withTenant $ \tid -> do
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
      notes <- lift $ getActivitiesByTagsTenant tid tags 1
      case notes of
        [] -> do
          -- For now, error - skill prompts should be seeded
          status status400
          json $ object ["error" .= ("Skill prompt not found. Run seeds first." :: Text)]
        (note:_) -> do
          -- Update existing prompt (stored in title field)
          lift $ updateActivityTitle (activityId note) (skillPrompt req)
          json $ object
            [ "status" .= ("updated" :: Text)
            , "skill" .= name
            , "note_id" .= activityId note
            ]
