module Skills.Reflection
  ( runProjectReflection
  , getProjectsNeedingReflection
  , buildReflectionPrompt
  , ReflectionResult(..)
  , detectProjectClusters
  , ProjectSuggestion(..)
  , ClusterKey(..)
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result(..), FromJSON(..), decode, fromJSON, toJSON, withObject, (.:?), (.!=))
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Domain.Id (EntityId)
import Domain.Document (Document(..), ExtendedProjectData(..), NewDocumentLogEntry(..), LogSource(..))
import Domain.Activity (Activity(..))
import Domain.ProjectSuggestion (ProjectSuggestion(..), ClusterKey(..))
import Domain.ActivityDocument (ActivityDocument(..))
import Infra.Db.Document (getAllProjects, updateProjectData, insertDocumentLog)
import Infra.Db.ActivityDocument (getDocumentActivities, getUnassignedActivities)
import Infra.Db.Activity (getActivity)
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))
import Infra.Claude.Client (callClaude)

data ReflectionResult = ReflectionResult
  { rrProjectId :: EntityId
  , rrUpdatedSummary :: Text
  , rrUpdatedStatus :: Text
  , rrActivitiesProcessed :: Int
  } deriving (Show)

-- | Get projects that have new activities since last reflection
getProjectsNeedingReflection :: App [Document]
getProjectsNeedingReflection = do
  projects <- getAllProjects
  -- Filter to those with recent activity links
  -- For now, return all active projects
  pure projects

-- | Build the reflection prompt for a project
buildReflectionPrompt :: Text -> Text -> [Text] -> Text
buildReflectionPrompt projectName currentSummary activitySummaries = T.unlines
  [ "You are updating a project's knowledge state based on recent activities."
  , ""
  , "## Current Project State"
  , "Name: " <> projectName
  , "Summary: " <> if T.null currentSummary then "(no summary yet)" else currentSummary
  , ""
  , "## Recent Activities"
  , if null activitySummaries
      then "(no new activities)"
      else T.unlines $ map ("- " <>) activitySummaries
  , ""
  , "## Instructions"
  , "Based on these activities, provide an updated project state."
  , "Synthesize the activities into a coherent summary."
  , "Infer the project status: 'active', 'stalled', or 'completed'."
  , ""
  , "Respond with ONLY a JSON object:"
  , "{"
  , "  \"summary\": \"Updated project summary...\","
  , "  \"status\": \"active|stalled|completed\""
  , "}"
  ]

-- | Run reflection for all projects
runProjectReflection :: App [ReflectionResult]
runProjectReflection = do
  projects <- getProjectsNeedingReflection
  results <- forM projects reflectOnProject
  pure $ concat results

-- | Reflect on a single project
reflectOnProject :: Document -> App [ReflectionResult]
reflectOnProject doc = do
  -- Get linked activities
  links <- getDocumentActivities (documentId doc) 50
  if null links
    then pure []
    else do
      -- Fetch activity details
      activities <- forM links $ \link -> getActivity (adActivityId link)
      let actSummaries = mapMaybe (\ma -> ma >>= activitySummary) (catMaybes [Just a | a <- activities])

      -- Get current project state
      case fromJSON (documentData doc) :: Result ExtendedProjectData of
        Error _ -> pure []
        Success projData -> do
          let prompt = buildReflectionPrompt
                (extProjectName projData)
                (extProjectSummary projData)
                actSummaries

          -- Call LLM
          cfg <- getConfig
          result <- liftIO $ callClaude (cfg.claude.apiKey) (cfg.claude.model) prompt

          case result of
            Left _ -> pure []
            Right respText -> do
              -- Parse response and update
              case parseReflectionResponse respText of
                Nothing -> pure []
                Just (newSummary, newStatus) -> do
                  let updatedData = projData
                        { extProjectSummary = newSummary
                        , extProjectStatus = newStatus
                        }

                  updateProjectData (documentId doc) (toJSON updatedData)

                  -- Log the reflection
                  let logEntry = NewDocumentLogEntry
                        { newLogDocumentId = documentId doc
                        , newLogActivityId = Nothing
                        , newLogDescription = Just $ "Reflection: processed " <> T.pack (show $ length links) <> " activities"
                        , newLogSource = LogReflection
                        , newLogAgentId = Nothing
                        }
                  _ <- insertDocumentLog logEntry

                  pure [ReflectionResult
                    { rrProjectId = documentId doc
                    , rrUpdatedSummary = newSummary
                    , rrUpdatedStatus = newStatus
                    , rrActivitiesProcessed = length links
                    }]

-- | Response type for reflection parsing
data ReflectionResponse = ReflectionResponse
  { respSummary :: Text
  , respStatus :: Text
  } deriving (Show)

instance FromJSON ReflectionResponse where
  parseJSON = withObject "ReflectionResponse" $ \v -> ReflectionResponse
    <$> v .:? "summary" .!= ""
    <*> v .:? "status" .!= "active"

-- | Parse the reflection response JSON
parseReflectionResponse :: Text -> Maybe (Text, Text)
parseReflectionResponse txt =
  case decode (TLE.encodeUtf8 $ TL.fromStrict txt) :: Maybe ReflectionResponse of
    Nothing -> Nothing
    Just r -> Just (respSummary r, respStatus r)

--------------------------------------------------------------------------------
-- Cluster Detection for New Project Suggestions
--------------------------------------------------------------------------------

-- | Minimum activities needed to suggest a new project
minClusterSize :: Int
minClusterSize = 5

-- | Detect clusters of unassigned activities that might represent new projects.
-- Groups activities by sender email domain and suggests projects for clusters
-- with at least minClusterSize activities.
detectProjectClusters :: App [ProjectSuggestion]
detectProjectClusters = do
  -- Get activities that have no project assignment
  unassigned <- getUnassignedActivities 500

  -- Group by sender email domain
  let clusters = groupByDomain unassigned

  -- Filter to clusters meeting minimum size and build suggestions
  let suggestions = mapMaybe buildSuggestion $ M.toList clusters

  -- Return sorted by activity count (largest first)
  pure $ sortOn (Down . psActivityCount) suggestions

-- | Group activities by sender email domain
groupByDomain :: [Activity] -> M.Map Text [Activity]
groupByDomain = foldr addToDomain M.empty
  where
    addToDomain act acc =
      case activitySenderEmail act of
        Nothing -> acc
        Just email ->
          let domain = extractDomain email
          in M.insertWith (++) domain [act] acc

-- | Extract domain from email address
extractDomain :: Text -> Text
extractDomain email =
  case T.splitOn "@" email of
    [_, domain] -> T.toLower domain
    _ -> "unknown"

-- | Build a project suggestion from a cluster if it meets minimum size
buildSuggestion :: (Text, [Activity]) -> Maybe ProjectSuggestion
buildSuggestion (domain, activities)
  | length activities < minClusterSize = Nothing
  | otherwise = Just ProjectSuggestion
      { psSuggestedName = suggestProjectName domain activities
      , psClusterKey = ClusterKey domain
      , psReason = "Found " <> T.pack (show $ length activities)
                   <> " activities from @" <> domain
      , psSampleActivityIds = take 5 $ map activityId activities
      , psActivityCount = length activities
      }

-- | Generate a suggested project name from domain and activities
suggestProjectName :: Text -> [Activity] -> Text
suggestProjectName domain activities =
  let -- Extract company name from domain (strip common TLDs)
      company = T.toTitle $ fromMaybe domain $ listToMaybe $ T.splitOn "." domain
      -- Try to find common themes in activity titles
      titles = mapMaybe activityTitle activities
      hasKeyword kw = any (T.isInfixOf (T.toLower kw) . T.toLower) titles
  in if hasKeyword "support" then company <> " Support"
     else if hasKeyword "sales" then company <> " Sales"
     else if hasKeyword "project" then company <> " Project"
     else if hasKeyword "contract" then company <> " Contract"
     else company <> " Engagement"
