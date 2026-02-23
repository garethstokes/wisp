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
    -- Execution
  , executeToolCall
  , getGitHubToken
    -- Agent metadata
  , agentInfo
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString.Base64 (decodeBase64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Domain.Agent (AgentInfo(..), ToolInfo(..), ToolType(..))
import Domain.Account (Account(..), AccountProvider(..))
import App.Monad (App)
import Infra.Db.Account (getAccountsByProvider)

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
-- GitHub API Client
--------------------------------------------------------------------------------

-- | Get access token from the first connected GitHub account
getGitHubToken :: App (Maybe Text)
getGitHubToken = do
  accounts <- getAccountsByProvider GitHub
  pure $ case accounts of
    [] -> Nothing
    (acc:_) -> extractAccessToken acc

extractAccessToken :: Account -> Maybe Text
extractAccessToken acc = case accountDetails acc of
  Aeson.Object obj -> case KM.lookup "access_token" obj of
    Just (Aeson.String t) -> Just t
    _ -> Nothing
  _ -> Nothing

-- | Make an authenticated request to GitHub API
githubRequest :: Text -> Text -> IO (Either Text Value)
githubRequest token endpoint = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com" <> T.unpack endpoint
  initReq <- parseRequest url
  let req = initReq
        { requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 token)
            , ("User-Agent", "wisp-srv")
            , ("Accept", "application/vnd.github+json")
            , ("X-GitHub-Api-Version", "2022-11-28")
            ]
        }
  response <- httpLbs req manager
  let status = statusCode (responseStatus response)
  case status of
    200 -> case Aeson.decode (responseBody response) of
      Just val -> pure $ Right val
      Nothing -> pure $ Left "Failed to parse GitHub response"
    404 -> pure $ Left "Not found"
    401 -> pure $ Left "GitHub authentication failed"
    403 -> pure $ Left "GitHub rate limit exceeded or forbidden"
    _ -> pure $ Left $ "GitHub API error: " <> T.pack (show status)

--------------------------------------------------------------------------------
-- Tool Execution
--------------------------------------------------------------------------------

-- | Execute a GitHub tool call
executeToolCall :: GitHubToolCall -> App ToolResult
executeToolCall call = do
  mToken <- getGitHubToken
  case mToken of
    Nothing -> pure $ ToolError "No GitHub account connected"
    Just token -> liftIO $ executeWithToken token call

executeWithToken :: Text -> GitHubToolCall -> IO ToolResult
executeWithToken token call = case call of
  ListRepos q -> do
    let limit = fromMaybe 20 (reposLimit q)
    result <- githubRequest token $ "/user/repos?per_page=" <> T.pack (show limit) <> "&sort=updated"
    pure $ toToolResult result

  ListCommits q -> do
    let limit = fromMaybe 10 (commitsLimit q)
        branchParam = maybe "" (\b -> "&sha=" <> b) (commitsBranch q)
        pathParam = maybe "" (\p -> "&path=" <> p) (commitsPath q)
    result <- githubRequest token $
      "/repos/" <> commitsRepo q <> "/commits?per_page=" <> T.pack (show limit) <> branchParam <> pathParam
    pure $ toToolResult result

  ReadFile q -> do
    let refParam = maybe "" (\r -> "?ref=" <> r) (readFileRef q)
    result <- githubRequest token $
      "/repos/" <> readFileRepo q <> "/contents/" <> readFilePath q <> refParam
    case result of
      Left err -> pure $ ToolError err
      Right val -> pure $ ToolSuccess $ decodeFileContent val

  ViewDiff q -> case (diffCommit q, diffBase q, diffHead q) of
    (Just sha, _, _) -> do
      -- View single commit diff
      result <- githubRequest token $ "/repos/" <> diffRepo q <> "/commits/" <> sha
      pure $ toToolResult result
    (_, Just base, Just headRef) -> do
      -- Compare two refs
      result <- githubRequest token $ "/repos/" <> diffRepo q <> "/compare/" <> base <> "..." <> headRef
      pure $ toToolResult result
    _ -> pure $ ToolError "view_diff requires either 'commit' or both 'base' and 'head'"

  ListPRs q -> do
    let limit = fromMaybe 10 (prsLimit q)
        state = fromMaybe "open" (prsState q)
    result <- githubRequest token $
      "/repos/" <> prsRepo q <> "/pulls?state=" <> state <> "&per_page=" <> T.pack (show limit)
    pure $ toToolResult result

  ViewPR q -> do
    result <- githubRequest token $
      "/repos/" <> prRepo q <> "/pulls/" <> T.pack (show (prNumber q))
    pure $ toToolResult result

toToolResult :: Either Text Value -> ToolResult
toToolResult (Left err) = ToolError err
toToolResult (Right val) = ToolSuccess val

-- | Decode base64 file content from GitHub API response
decodeFileContent :: Value -> Value
decodeFileContent val = case val of
  Aeson.Object obj -> case KM.lookup "content" obj of
    Just (Aeson.String encoded) ->
      -- GitHub returns base64-encoded content with newlines
      let cleaned = T.filter (/= '\n') encoded
          decoded = case decodeBase64 (encodeUtf8 cleaned) of
            Right bs -> decodeUtf8 bs
            Left _ -> encoded  -- Return original if decode fails
      in Aeson.Object $ KM.insert "content" (Aeson.String decoded) obj
    _ -> val
  _ -> val

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
