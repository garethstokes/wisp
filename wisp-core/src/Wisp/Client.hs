module Wisp.Client
  ( -- * Client operations
    getActivities
  , getActivitiesPage
  , ActivitiesResponse(..)
  , getActivity
  , getDocuments
  , getProjects
  , getNotes
  , getPreferences
  , createProject
  , createNote
  , setPreference
  , archiveDocument
  , approveActivity
  , dismissActivity
  , getApprovals
  , ApprovalItem(..)
    -- * Skills and Agents
  , getSkills
  , getAgents
  , getAgent
  , getAgentSessions
    -- * Re-exports
  , module Wisp.Client.Types
  , module Wisp.Client.Activities
  , module Wisp.Client.Documents
  ) where

import Control.Exception (try, SomeException)
import Data.Aeson (FromJSON, ToJSON, decode, encode, object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)

import Wisp.Client.Types
import Wisp.Client.Activities
import Wisp.Client.Documents

-- | Make a GET request
httpGet :: FromJSON a => ClientConfig -> String -> IO (Either ClientError a)
httpGet cfg path = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    req <- parseRequest $ T.unpack (configBaseUrl cfg) <> path
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right response ->
      let code = statusCode $ responseStatus response
      in if code >= 200 && code < 300
         then case decode (responseBody response) of
           Just a -> pure $ Right a
           Nothing -> pure $ Left $ ParseError "Failed to parse response"
         else pure $ Left $ ServerError code $ T.pack $ show $ responseBody response

-- | Make a POST request with JSON body
httpPost :: (ToJSON a, FromJSON b) => ClientConfig -> String -> a -> IO (Either ClientError b)
httpPost cfg path body = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    initialReq <- parseRequest $ T.unpack (configBaseUrl cfg) <> path
    let req = initialReq
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (encode body)
          }
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right response ->
      let code = statusCode $ responseStatus response
      in if code >= 200 && code < 300
         then case decode (responseBody response) of
           Just a -> pure $ Right a
           Nothing -> pure $ Left $ ParseError "Failed to parse response"
         else pure $ Left $ ServerError code $ T.pack $ show $ responseBody response

-- | POST without expecting response body
httpPost_ :: ToJSON a => ClientConfig -> String -> a -> IO (Either ClientError ())
httpPost_ cfg path body = do
  manager <- newManager defaultManagerSettings
  result <- try $ do
    initialReq <- parseRequest $ T.unpack (configBaseUrl cfg) <> path
    let req = initialReq
          { method = "POST"
          , requestHeaders = [("Content-Type", "application/json")]
          , requestBody = RequestBodyLBS (encode body)
          }
    httpLbs req manager
  case result of
    Left (e :: SomeException) -> pure $ Left $ HttpError $ T.pack $ show e
    Right response ->
      let code = statusCode $ responseStatus response
      in if code >= 200 && code < 300
         then pure $ Right ()
         else pure $ Left $ ServerError code $ T.pack $ show $ responseBody response

-- | Response from getActivities including both activities and metrics
data ActivitiesResponse = ActivitiesResponse
  { arActivities :: [Activity]
  , arMetrics :: Maybe ActivityMetrics
  , arHasMore :: Bool
  , arOffset :: Int
  } deriving (Show, Eq)

-- Activities (first page)
getActivities :: ClientConfig -> IO (Either ClientError ActivitiesResponse)
getActivities cfg = getActivitiesPage cfg 50 0

-- Activities with pagination
getActivitiesPage :: ClientConfig -> Int -> Int -> IO (Either ClientError ActivitiesResponse)
getActivitiesPage cfg limit offset = do
  let path = "/activities?limit=" <> show limit <> "&offset=" <> show offset
  result <- httpGet cfg path
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "activities" obj of
        Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
          Aeson.Success activities -> Right ActivitiesResponse
            { arActivities = toList activities
            , arMetrics = case KM.lookup "metrics" obj of
                Just m -> case Aeson.fromJSON m of
                  Aeson.Success metrics -> Just metrics
                  _ -> Nothing
                _ -> Nothing
            , arHasMore = case KM.lookup "has_more" obj of
                Just (Aeson.Bool b) -> b
                _ -> False
            , arOffset = case KM.lookup "offset" obj of
                Just (Aeson.Number n) -> round n
                _ -> 0
            }
          _ -> Left $ ParseError "Failed to parse activities array"
        _ -> Left $ ParseError "Missing activities key"
      _ -> Left $ ParseError "Expected object"

getActivity :: ClientConfig -> Text -> IO (Either ClientError Activity)
getActivity cfg aid = httpGet cfg $ "/activities/" <> T.unpack aid

-- Documents
getDocuments :: ClientConfig -> DocumentType -> IO (Either ClientError [Document])
getDocuments cfg docType = do
  let path = case docType of
        ProjectDoc -> "/api/projects"
        NoteDoc -> "/api/notes"
        PreferenceDoc -> "/api/preferences"
  result <- httpGet cfg path
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj ->
        let key = case docType of
              ProjectDoc -> "projects"
              NoteDoc -> "notes"
              PreferenceDoc -> "preferences"
        in case KM.lookup key obj of
          Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
            Aeson.Success docs -> Right $ toList docs
            _ -> Left $ ParseError "Failed to parse documents"
          _ -> Left $ ParseError "Missing documents key"
      _ -> Left $ ParseError "Expected object"

getProjects :: ClientConfig -> IO (Either ClientError [Document])
getProjects cfg = getDocuments cfg ProjectDoc

getNotes :: ClientConfig -> IO (Either ClientError [Document])
getNotes cfg = getDocuments cfg NoteDoc

getPreferences :: ClientConfig -> IO (Either ClientError [Document])
getPreferences cfg = getDocuments cfg PreferenceDoc

createProject :: ClientConfig -> Text -> Text -> IO (Either ClientError Text)
createProject cfg name projType = do
  result <- httpPost cfg "/api/projects" $ object ["name" .= name, "type" .= projType]
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> case KM.lookup "id" obj of
      Just (Aeson.String pid) -> Right pid
      _ -> Left $ ParseError "Missing id in response"
    Right _ -> Left $ ParseError "Expected object"

createNote :: ClientConfig -> Text -> Maybe Text -> [Text] -> IO (Either ClientError Text)
createNote cfg title mContent tags = do
  let body = object $ ["title" .= title]
        <> maybe [] (\c -> ["content" .= c]) mContent
        <> if null tags then [] else ["tags" .= tags]
  result <- httpPost cfg "/api/notes" body
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> case KM.lookup "id" obj of
      Just (Aeson.String nid) -> Right nid
      _ -> Left $ ParseError "Missing id"
    Right _ -> Left $ ParseError "Expected object"

setPreference :: ClientConfig -> Text -> Text -> Maybe Text -> IO (Either ClientError Text)
setPreference cfg key value mContext = do
  let body = object $ ["key" .= key, "value" .= value]
        <> maybe [] (\c -> ["context" .= c]) mContext
  result <- httpPost cfg "/api/preferences" body
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> case KM.lookup "id" obj of
      Just (Aeson.String pid) -> Right pid
      _ -> Left $ ParseError "Missing id"
    Right _ -> Left $ ParseError "Expected object"

archiveDocument :: ClientConfig -> Text -> IO (Either ClientError ())
archiveDocument cfg docId =
  httpPost_ cfg ("/api/projects/" <> T.unpack docId <> "/archive") (object [])

-- Approvals
data ApprovalItem = ApprovalItem
  { approvalActivity :: Activity
  , approvalType :: Text  -- "quarantine" or "classify"
  , approvalReason :: Text
  } deriving (Show, Eq)

instance FromJSON ApprovalItem where
  parseJSON = Aeson.withObject "ApprovalItem" $ \v -> ApprovalItem
    <$> v Aeson..: "activity"
    <*> v Aeson..: "type"
    <*> v Aeson..: "reason"

getApprovals :: ClientConfig -> IO (Either ClientError [ApprovalItem])
getApprovals cfg = do
  -- Fetch inbox items that need attention
  result <- httpGet cfg "/inbox"
  pure $ case result of
    Left e -> Left e
    Right (Aeson.Object obj) -> Right $ concat
      [ extractItems "quarantine" "quarantined" obj
      , extractItems "high_urgency" "high_urgency" obj
      , extractItems "surfaced" "surfaced" obj
      ]
    Right _ -> Left $ ParseError "Expected object"
  where
    extractItems aType key obj = case KM.lookup key obj of
      Just (Aeson.Array arr) ->
        [ ApprovalItem act aType "needs attention"
        | Aeson.Object actObj <- toList arr
        , Aeson.Success act <- [Aeson.fromJSON (Aeson.Object actObj)]
        ]
      _ -> []

approveActivity :: ClientConfig -> Text -> IO (Either ClientError ())
approveActivity cfg aid =
  httpPost_ cfg ("/activities/" <> T.unpack aid <> "/approve") (object [])

dismissActivity :: ClientConfig -> Text -> IO (Either ClientError ())
dismissActivity cfg aid =
  httpPost_ cfg ("/activities/" <> T.unpack aid <> "/dismiss") (object [])

-- Skills
getSkills :: ClientConfig -> IO (Either ClientError [Skill])
getSkills cfg = do
  result <- httpGet cfg "/api/skills"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "skills" obj of
        Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
          Aeson.Success skills -> Right $ toList skills
          _ -> Left $ ParseError "Failed to parse skills"
        _ -> Left $ ParseError "Missing skills key"
      _ -> Left $ ParseError "Expected object"

-- Agents
getAgents :: ClientConfig -> IO (Either ClientError [Text])
getAgents cfg = do
  result <- httpGet cfg "/api/agents"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "agents" obj of
        Just (Aeson.Array arr) -> Right
          [ name | Aeson.String name <- toList arr ]
        _ -> Left $ ParseError "Missing agents key"
      _ -> Left $ ParseError "Expected object"

getAgent :: ClientConfig -> Text -> IO (Either ClientError AgentInfo)
getAgent cfg name = httpGet cfg $ "/api/agents/" <> T.unpack name

getAgentSessions :: ClientConfig -> Text -> IO (Either ClientError [SessionSummary])
getAgentSessions cfg name = do
  result <- httpGet cfg $ "/api/agents/" <> T.unpack name <> "/sessions"
  pure $ case result of
    Left e -> Left e
    Right val -> case val of
      Aeson.Object obj -> case KM.lookup "sessions" obj of
        Just (Aeson.Array arr) -> case traverse Aeson.fromJSON arr of
          Aeson.Success sessions -> Right $ toList sessions
          _ -> Left $ ParseError "Failed to parse sessions"
        _ -> Left $ ParseError "Missing sessions key"
      _ -> Left $ ParseError "Expected object"

-- Helper
toList :: Foldable t => t a -> [a]
toList = foldr (:) []
