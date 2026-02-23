-- | GitHub Skill
-- Read-only access to GitHub repositories using existing OAuth accounts.
module Skills.GitHub
  ( -- Types
    GitHubToolCall(..)
  , ListReposQuery(..)
  , ListCommitsQuery(..)
  , ReadFileQuery(..)
  , ViewDiffQuery(..)
  , ListPRsQuery(..)
  , ViewPRQuery(..)
  , ToolResult(..)
    -- Agent metadata
  , agentInfo
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))

--------------------------------------------------------------------------------
-- Tool Call Types
--------------------------------------------------------------------------------

data GitHubToolCall
  = ListRepos ListReposQuery
  | ListCommits ListCommitsQuery
  | ReadFile ReadFileQuery
  | ViewDiff ViewDiffQuery
  | ListPRs ListPRsQuery
  | ViewPR ViewPRQuery
  deriving (Show, Eq)

data ListReposQuery = ListReposQuery
  { reposLimit :: Maybe Int
  } deriving (Show, Eq)

data ListCommitsQuery = ListCommitsQuery
  { commitsRepo   :: Text          -- "owner/repo"
  , commitsBranch :: Maybe Text    -- default: repo default branch
  , commitsPath   :: Maybe Text    -- filter by file path
  , commitsLimit  :: Maybe Int     -- default: 10
  } deriving (Show, Eq)

data ReadFileQuery = ReadFileQuery
  { readFileRepo :: Text     -- "owner/repo"
  , readFilePath :: Text     -- file path within repo
  , readFileRef  :: Maybe Text  -- branch/commit/tag (default: repo default)
  } deriving (Show, Eq)

data ViewDiffQuery = ViewDiffQuery
  { diffRepo   :: Text         -- "owner/repo"
  , diffCommit :: Maybe Text   -- Single commit SHA (view this commit's changes)
  , diffBase   :: Maybe Text   -- For compare: base ref
  , diffHead   :: Maybe Text   -- For compare: head ref
  } deriving (Show, Eq)

data ListPRsQuery = ListPRsQuery
  { prsRepo  :: Text           -- "owner/repo"
  , prsState :: Maybe Text     -- "open", "closed", "all" (default: "open")
  , prsLimit :: Maybe Int      -- default: 10
  } deriving (Show, Eq)

data ViewPRQuery = ViewPRQuery
  { prRepo   :: Text   -- "owner/repo"
  , prNumber :: Int    -- PR number
  } deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Tool Result
--------------------------------------------------------------------------------

data ToolResult
  = ToolSuccess Value
  | ToolError Text
  deriving (Show, Eq)

instance ToJSON ToolResult where
  toJSON (ToolSuccess v) = object ["success" .= True, "data" .= v]
  toJSON (ToolError err) = object ["success" .= False, "error" .= err]

--------------------------------------------------------------------------------
-- JSON Instances (FromJSON)
--------------------------------------------------------------------------------

instance FromJSON GitHubToolCall where
  parseJSON = withObject "GitHubToolCall" $ \v -> do
    tool <- v .: "tool"
    case (tool :: Text) of
      "list_repos"   -> ListRepos <$> parseJSON (Object v)
      "list_commits" -> ListCommits <$> parseJSON (Object v)
      "read_file"    -> ReadFile <$> parseJSON (Object v)
      "view_diff"    -> ViewDiff <$> parseJSON (Object v)
      "list_prs"     -> ListPRs <$> parseJSON (Object v)
      "view_pr"      -> ViewPR <$> parseJSON (Object v)
      _ -> fail $ "Unknown GitHub tool: " <> T.unpack tool

instance FromJSON ListReposQuery where
  parseJSON = withObject "ListReposQuery" $ \v -> ListReposQuery
    <$> v .:? "limit"

instance FromJSON ListCommitsQuery where
  parseJSON = withObject "ListCommitsQuery" $ \v -> ListCommitsQuery
    <$> v .: "repo"
    <*> v .:? "branch"
    <*> v .:? "path"
    <*> v .:? "limit"

instance FromJSON ReadFileQuery where
  parseJSON = withObject "ReadFileQuery" $ \v -> ReadFileQuery
    <$> v .: "repo"
    <*> v .: "path"
    <*> v .:? "ref"

instance FromJSON ViewDiffQuery where
  parseJSON = withObject "ViewDiffQuery" $ \v -> ViewDiffQuery
    <$> v .: "repo"
    <*> v .:? "commit"
    <*> v .:? "base"
    <*> v .:? "head"

instance FromJSON ListPRsQuery where
  parseJSON = withObject "ListPRsQuery" $ \v -> ListPRsQuery
    <$> v .: "repo"
    <*> v .:? "state"
    <*> v .:? "limit"

instance FromJSON ViewPRQuery where
  parseJSON = withObject "ViewPRQuery" $ \v -> ViewPRQuery
    <$> v .: "repo"
    <*> v .: "number"

--------------------------------------------------------------------------------
-- JSON Instances (ToJSON)
--------------------------------------------------------------------------------

instance ToJSON GitHubToolCall where
  toJSON (ListRepos q) = object
    [ "tool" .= ("list_repos" :: Text)
    , "limit" .= reposLimit q
    ]
  toJSON (ListCommits q) = object
    [ "tool" .= ("list_commits" :: Text)
    , "repo" .= commitsRepo q
    , "branch" .= commitsBranch q
    , "path" .= commitsPath q
    , "limit" .= commitsLimit q
    ]
  toJSON (ReadFile q) = object
    [ "tool" .= ("read_file" :: Text)
    , "repo" .= readFileRepo q
    , "path" .= readFilePath q
    , "ref" .= readFileRef q
    ]
  toJSON (ViewDiff q) = object
    [ "tool" .= ("view_diff" :: Text)
    , "repo" .= diffRepo q
    , "commit" .= diffCommit q
    , "base" .= diffBase q
    , "head" .= diffHead q
    ]
  toJSON (ListPRs q) = object
    [ "tool" .= ("list_prs" :: Text)
    , "repo" .= prsRepo q
    , "state" .= prsState q
    , "limit" .= prsLimit q
    ]
  toJSON (ViewPR q) = object
    [ "tool" .= ("view_pr" :: Text)
    , "repo" .= prRepo q
    , "number" .= prNumber q
    ]

instance ToJSON ListReposQuery where
  toJSON ListReposQuery {..} = object ["limit" .= reposLimit]

instance ToJSON ListCommitsQuery where
  toJSON ListCommitsQuery {..} = object
    [ "repo" .= commitsRepo
    , "branch" .= commitsBranch
    , "path" .= commitsPath
    , "limit" .= commitsLimit
    ]

instance ToJSON ReadFileQuery where
  toJSON ReadFileQuery {..} = object
    [ "repo" .= readFileRepo
    , "path" .= readFilePath
    , "ref" .= readFileRef
    ]

instance ToJSON ViewDiffQuery where
  toJSON ViewDiffQuery {..} = object
    [ "repo" .= diffRepo
    , "commit" .= diffCommit
    , "base" .= diffBase
    , "head" .= diffHead
    ]

instance ToJSON ListPRsQuery where
  toJSON ListPRsQuery {..} = object
    [ "repo" .= prsRepo
    , "state" .= prsState
    , "limit" .= prsLimit
    ]

instance ToJSON ViewPRQuery where
  toJSON ViewPRQuery {..} = object
    [ "repo" .= prRepo
    , "number" .= prNumber
    ]

--------------------------------------------------------------------------------
-- Agent Metadata
--------------------------------------------------------------------------------

agentInfo :: AgentInfo
agentInfo = AgentInfo
  { agentId = "github"
  , agentDescription = "Read-only access to GitHub repositories via connected OAuth accounts"
  , agentTools =
      [ ToolInfo "list_repos" Decision
      , ToolInfo "list_commits" Decision
      , ToolInfo "read_file" Decision
      , ToolInfo "view_diff" Decision
      , ToolInfo "list_prs" Decision
      , ToolInfo "view_pr" Decision
      ]
  , agentWorkflows = ["code-review", "repository-exploration", "commit-history"]
  , agentImplemented = True
  }
