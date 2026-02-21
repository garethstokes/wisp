module Http.Handlers.Agents
  ( getAgentsList
  , getAgentByName
  , getAgentSessions
  , postAgentActivateSkill
  , postAgentDeactivate
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, withObject, (.:), (.=))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status400, status404, status500)
import Web.Scotty.Trans (ActionT, json, status, jsonData, captureParam)
import App.Monad (Env)
import Agents.Dispatcher (listAgentNamesByTenant)
import Agents.Core (Agent(..), loadAgentByTenant)
import Skills.Base (activateAgentSkillByTenant, deactivateAgentSkillByTenant)
import Skills.Registry (Skill(..), allSkillNames)
import Domain.Agent (AgentConfig(..))
import Domain.Soul (Soul(..))
import Domain.Tenant (TenantId, Tenant(..))
import Infra.Db.Tenant (getAllTenants)
import Infra.Db.Session (getRecentSessions)

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
-- Agent Endpoints
--------------------------------------------------------------------------------

-- | GET /api/agents - list agent names from knowledge
getAgentsList :: ActionT (ReaderT Env IO) ()
getAgentsList = withTenant $ \tid -> do
  names <- lift $ listAgentNamesByTenant tid
  json $ object
    [ "agents" .= names
    , "count" .= length names
    ]

-- | GET /api/agents/:name - get agent details
getAgentByName :: ActionT (ReaderT Env IO) ()
getAgentByName = withTenant $ \tid -> do
  name <- captureParam "name"
  mAgent <- lift $ loadAgentByTenant tid name
  case mAgent of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Agent not found: " <> name :: Text)]
    Just agent -> do
      json $ agentToJson agent

-- | Agent JSON representation
agentToJson :: Agent -> Value
agentToJson agent = object
  [ "name" .= agentName agent
  , "personality" .= agentPersonalitySeed (agentConfig agent)
  , "active_skill" .= fmap skillName (agentSkill agent)
  , "soul" .= soulToJson (agentSoul agent)
  , "available_skills" .= allSkillNames
  ]

soulToJson :: Soul -> Value
soulToJson soul = object
  [ "personality" .= soulPersonality soul
  , "insights" .= soulInsights soul
  ]

-- | Request body for skill activation
data ActivateSkillRequest = ActivateSkillRequest
  { activateConfirm :: Bool  -- User must confirm
  } deriving (Show, Eq, Generic)

instance FromJSON ActivateSkillRequest where
  parseJSON = withObject "ActivateSkillRequest" $ \v ->
    ActivateSkillRequest <$> v .: "confirm"

instance ToJSON ActivateSkillRequest where
  toJSON req = object ["confirm" .= activateConfirm req]

-- | GET /api/agents/:name/sessions - get recent sessions
getAgentSessions :: ActionT (ReaderT Env IO) ()
getAgentSessions = withTenant $ \_ -> do
  name <- captureParam "name"
  sessions <- lift $ getRecentSessions name 10
  json $ object
    [ "sessions" .= sessions
    , "count" .= length sessions
    ]

-- | POST /api/agents/:name/activate/:skill - activate a skill
postAgentActivateSkill :: ActionT (ReaderT Env IO) ()
postAgentActivateSkill = withTenant $ \tid -> do
  name <- captureParam "name"
  skillToActivate <- captureParam "skill"
  req <- jsonData :: ActionT (ReaderT Env IO) ActivateSkillRequest

  if not (activateConfirm req)
    then do
      status status400
      json $ object
        [ "error" .= ("Confirmation required" :: Text)
        , "message" .= ("Set confirm: true to activate skill" :: Text)
        ]
    else do
      result <- lift $ activateAgentSkillByTenant tid name skillToActivate
      case result of
        Left err -> do
          status status400
          json $ object ["error" .= err]
        Right () -> do
          json $ object
            [ "status" .= ("activated" :: Text)
            , "agent" .= name
            , "skill" .= skillToActivate
            ]

-- | POST /api/agents/:name/deactivate - deactivate current skill
postAgentDeactivate :: ActionT (ReaderT Env IO) ()
postAgentDeactivate = withTenant $ \tid -> do
  name <- captureParam "name"

  result <- lift $ deactivateAgentSkillByTenant tid name
  case result of
    Left err -> do
      status status400
      json $ object ["error" .= err]
    Right () -> do
      json $ object
        [ "status" .= ("deactivated" :: Text)
        , "agent" .= name
        ]
