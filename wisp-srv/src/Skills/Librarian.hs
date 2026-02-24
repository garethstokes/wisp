-- | Librarian Skill
-- Maintains project knowledge by synthesizing activities and GitHub data.
module Skills.Librarian
  ( runLibrarian
  , runLibrarianForProject
  , LibrarianResult(..)
  ) where

import Control.Monad (forM, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Result(..), FromJSON(..), ToJSON(..), Value(..), eitherDecode, fromJSON, toJSON, withObject, (.:?))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Time (UTCTime, getCurrentTime)
import Domain.Id (EntityId)
import Domain.Document (Document(..), DocumentType(..), NewDocument(..), ExtendedProjectData(..))
import Domain.Activity (Activity(..))
import Domain.ActivityDocument (ActivityDocument(..))
import Domain.ProjectKnowledge
import Infra.Db.Document (getAllProjects, getProjectChildren, insertDocument, insertWithSupersedes)
import Infra.Db.ActivityDocument (getDocumentActivities)
import Infra.Db.Activity (getActivity)
import App.Monad (App, getConfig)
import App.Config (Config(..), ClaudeConfig(..))
import Infra.Claude.Client (callClaude)
import qualified Skills.GitHub as GitHub

--------------------------------------------------------------------------------
-- Result Type
--------------------------------------------------------------------------------

data LibrarianResult = LibrarianResult
  { lrProjectId :: EntityId
  , lrProjectName :: Text
  , lrUpdatedDocs :: [ProjectKnowledgeKind]
  , lrSkippedDocs :: [ProjectKnowledgeKind]
  , lrLogs :: [Text]  -- Execution logs for debugging
  } deriving (Show)

--------------------------------------------------------------------------------
-- Main Entry Points
--------------------------------------------------------------------------------

-- | Run librarian for all active projects
runLibrarian :: App [LibrarianResult]
runLibrarian = do
  projects <- getAllProjects
  results <- forM projects runLibrarianForProject
  pure $ catMaybes results

-- | Run librarian for a single project
runLibrarianForProject :: Document -> App (Maybe LibrarianResult)
runLibrarianForProject doc = do
  -- Parse project data
  case fromJSON (documentData doc) :: Result ExtendedProjectData of
    Error err -> do
      let errLog = "Failed to parse project data: " <> T.pack err
      liftIO $ putStrLn $ "[Librarian] " <> T.unpack errLog
      pure $ Just LibrarianResult
        { lrProjectId = documentId doc
        , lrProjectName = "(parse error)"
        , lrUpdatedDocs = []
        , lrSkippedDocs = []
        , lrLogs = [errLog]
        }
    Success projData -> do
      let log1 = "Processing project: " <> extProjectName projData
      liftIO $ putStrLn $ "[Librarian] " <> T.unpack log1

      -- Get linked activities
      links <- getDocumentActivities (documentId doc) 100
      let log2 = "Found " <> T.pack (show (length links)) <> " linked activities"
      liftIO $ putStrLn $ "[Librarian] " <> T.unpack log2

      if null links
        then do
          let log3 = "No linked activities, skipping"
          liftIO $ putStrLn $ "[Librarian] " <> T.unpack log3
          pure $ Just LibrarianResult
            { lrProjectId = documentId doc
            , lrProjectName = extProjectName projData
            , lrUpdatedDocs = []
            , lrSkippedDocs = [ProductResearch, Roadmap, Architecture, ActivityLog]
            , lrLogs = [log1, log2, log3]
            }
        else do
          -- Fetch activity details
          activities <- forM links $ \link -> getActivity (adActivityId link)
          let validActivities = catMaybes activities
          let log3 = "Loaded " <> T.pack (show (length validActivities)) <> " activity details"
          liftIO $ putStrLn $ "[Librarian] " <> T.unpack log3

          -- Sample activities for the log
          let sampleTitles = T.intercalate ", " $ take 3 $ mapMaybe activityTitle validActivities
          let log3b = "Sample activities: " <> sampleTitles
          liftIO $ putStrLn $ "[Librarian] " <> T.unpack log3b

          -- Get existing child knowledge docs
          children <- getProjectChildren (documentId doc)
          let log4 = "Found " <> T.pack (show (length children)) <> " existing child docs"
          liftIO $ putStrLn $ "[Librarian] " <> T.unpack log4

          -- Check for GitHub enrichment
          let mRepoTag = findRepoTag (documentTags doc)
          (githubData, log5) <- case mRepoTag of
            Nothing -> do
              let l = "No repo tag, skipping GitHub enrichment"
              liftIO $ putStrLn $ "[Librarian] " <> T.unpack l
              pure (Nothing, l)
            Just repo -> do
              let l = "Fetching GitHub data for: " <> repo
              liftIO $ putStrLn $ "[Librarian] " <> T.unpack l
              d <- getGitHubEnrichment repo
              pure (d, l)

          -- Build and call LLM
          let prompt = buildLibrarianPrompt projData validActivities children githubData
          let log6 = "Built prompt (" <> T.pack (show (T.length prompt)) <> " chars)"
          liftIO $ putStrLn $ "[Librarian] " <> T.unpack log6

          let log7 = "Calling Claude API..."
          liftIO $ putStrLn $ "[Librarian] " <> T.unpack log7

          cfg <- getConfig
          result <- liftIO $ callClaude (cfg.claude.apiKey) (cfg.claude.model) prompt

          case result of
            Left err -> do
              let log8 = "Claude API error: " <> err
              liftIO $ putStrLn $ "[Librarian] " <> T.unpack log8
              pure $ Just LibrarianResult
                { lrProjectId = documentId doc
                , lrProjectName = extProjectName projData
                , lrUpdatedDocs = []
                , lrSkippedDocs = [ProductResearch, Roadmap, Architecture, ActivityLog]
                , lrLogs = [log1, log2, log3, log3b, log4, log5, log6, log7, log8]
                }
            Right respText -> do
              let log8 = "Got response (" <> T.pack (show (T.length respText)) <> " chars)"
              liftIO $ putStrLn $ "[Librarian] " <> T.unpack log8

              let log9 = "Response preview: " <> T.take 300 respText
              liftIO $ putStrLn $ "[Librarian] " <> T.unpack log9

              -- Parse and persist updates
              (updated, skipped, parseError) <- parseAndPersistUpdates (documentId doc) children respText
              let log10 = if T.null parseError
                          then "Parse successful"
                          else "Parse error: " <> parseError
              when (not $ T.null parseError) $
                liftIO $ putStrLn $ "[Librarian] " <> T.unpack log10

              let log11 = "Result: Updated " <> T.pack (show (length updated)) <> ", Skipped " <> T.pack (show (length skipped))
              liftIO $ putStrLn $ "[Librarian] " <> T.unpack log11

              pure $ Just LibrarianResult
                { lrProjectId = documentId doc
                , lrProjectName = extProjectName projData
                , lrUpdatedDocs = updated
                , lrSkippedDocs = skipped
                , lrLogs = [log1, log2, log3, log3b, log4, log5, log6, log7, log8, log9, log10, log11]
                }

--------------------------------------------------------------------------------
-- GitHub Enrichment
--------------------------------------------------------------------------------

-- | Find repo: tag from document tags
findRepoTag :: [Text] -> Maybe Text
findRepoTag tags =
  listToMaybe $ mapMaybe extractRepo tags
  where
    extractRepo tag
      | "repo:" `T.isPrefixOf` tag = Just $ T.drop 5 tag
      | otherwise = Nothing

-- | Get recent commits from GitHub for enrichment
getGitHubEnrichment :: Text -> App (Maybe Text)
getGitHubEnrichment repo = do
  result <- GitHub.executeToolCall (GitHub.ListCommits $ GitHub.ListCommitsQuery
    { GitHub.commitsRepo = repo
    , GitHub.commitsBranch = Nothing
    , GitHub.commitsPath = Nothing
    , GitHub.commitsLimit = Just 10
    })
  case result of
    GitHub.ToolSuccess val -> pure $ Just $ formatGitHubData val
    GitHub.ToolError _ -> pure Nothing

-- | Format GitHub API response for the prompt
formatGitHubData :: Value -> Text
formatGitHubData val = case val of
  Array commits -> T.unlines $ map formatCommit (take 10 $ foldr (:) [] commits)
  _ -> "(unable to parse GitHub data)"
  where
    formatCommit :: Value -> Text
    formatCommit (Object obj) =
      let sha = fromMaybe "" $ extractText "sha" obj
          msg = fromMaybe "" $ extractNestedText ["commit", "message"] obj
          author = fromMaybe "" $ extractNestedText ["commit", "author", "name"] obj
      in "- " <> T.take 7 sha <> " (" <> author <> "): " <> T.takeWhile (/= '\n') msg
    formatCommit _ = ""

    extractText :: Text -> KM.KeyMap Value -> Maybe Text
    extractText k obj = case KM.lookup (Key.fromText k) obj of
      Just (String t) -> Just t
      _ -> Nothing

    extractNestedText :: [Text] -> KM.KeyMap Value -> Maybe Text
    extractNestedText [] _ = Nothing
    extractNestedText [k] obj = extractText k obj
    extractNestedText (k:ks) obj = case KM.lookup (Key.fromText k) obj of
      Just (Object o) -> extractNestedText ks o
      _ -> Nothing

--------------------------------------------------------------------------------
-- LLM Prompt Building
--------------------------------------------------------------------------------

-- | Build the librarian prompt
buildLibrarianPrompt :: ExtendedProjectData -> [Activity] -> [Document] -> Maybe Text -> Text
buildLibrarianPrompt projData activities children mGithubData = T.unlines
  [ "You are a project librarian maintaining knowledge documents for a project."
  , ""
  , "## Project"
  , "Name: " <> extProjectName projData
  , "Summary: " <> if T.null (extProjectSummary projData) then "(no summary)" else extProjectSummary projData
  , "Status: " <> extProjectStatus projData
  , ""
  , "## Current Knowledge Documents"
  , ""
  , "### Product Research"
  , formatKnowledgeDoc children ProductResearch
  , ""
  , "### Roadmap"
  , formatKnowledgeDoc children Roadmap
  , ""
  , "### Architecture"
  , formatKnowledgeDoc children Architecture
  , ""
  , "### Activity Log"
  , formatKnowledgeDoc children ActivityLog
  , ""
  , "## New Activities"
  , if null activities
      then "(no new activities)"
      else T.unlines $ map formatActivity activities
  , ""
  , case mGithubData of
      Nothing -> ""
      Just ghData -> T.unlines
        [ "## GitHub Commits"
        , ghData
        , ""
        ]
  , "## Instructions"
  , "Your task is to CREATE or UPDATE project knowledge documents based on the activities above."
  , ""
  , "IMPORTANT RULES:"
  , "1. If a document shows '(Not yet created)', you MUST CREATE it by providing complete content."
  , "2. If a document exists but the activities contain relevant new information, UPDATE it."
  , "3. Only use \"no_change\" if a document already exists AND no new activities are relevant to it."
  , "4. You have " <> T.pack (show (length activities)) <> " activities to process - there IS useful information here."
  , ""
  , "For this project, synthesize the activities into knowledge documents. Extract:"
  , "- Product Research: vision, value proposition, key people/contacts mentioned, open questions"
  , "- Roadmap: milestones, timeline, goals mentioned in activities"
  , "- Architecture: technical decisions, code patterns, infrastructure mentioned"
  , "- Activity Log: summary of what happened, key highlights"
  , ""
  , "Respond with ONLY a JSON object with this structure:"
  , "{"
  , "  \"product_research\": { ... document data ... } | \"no_change\","
  , "  \"roadmap\": { ... document data ... } | \"no_change\","
  , "  \"architecture\": { ... document data ... } | \"no_change\","
  , "  \"activity_log\": { ... document data ... } | \"no_change\""
  , "}"
  , ""
  , "REMEMBER: Documents marked '(Not yet created)' MUST have content provided, not \"no_change\"."
  , ""
  , "For each document type, include all fields:"
  , ""
  , "product_research: { \"vision\": \"...\", \"value_proposition\": \"...\", \"key_contacts\": [...], \"open_questions\": [...] }"
  , "roadmap: { \"milestones\": [...], \"timeline_notes\": \"...\" }"
  , "architecture: { \"users_personas\": \"...\", \"specs_links\": \"...\", \"testing\": \"...\", \"code_structure\": \"...\", \"data_structure\": \"...\", \"infrastructure\": \"...\" }"
  , "activity_log: { \"period\": \"...\", \"summary\": \"...\", \"highlights\": [...] }"
  ]

-- | Format a knowledge document for the prompt
formatKnowledgeDoc :: [Document] -> ProjectKnowledgeKind -> Text
formatKnowledgeDoc children kind =
  case findChildByKind children kind of
    Nothing -> "(Not yet created)"
    Just doc -> formatDocData (documentData doc) kind

-- | Find child document by kind
findChildByKind :: [Document] -> ProjectKnowledgeKind -> Maybe Document
findChildByKind children kind =
  listToMaybe $ filter (hasKind kind) children
  where
    hasKind k doc = case documentData doc of
      Object obj -> case KM.lookup (Key.fromText "kind") obj of
        Just (String kindStr) -> kindStr == kindToText k
        _ -> False
      _ -> False

    kindToText :: ProjectKnowledgeKind -> Text
    kindToText ProductResearch = "product_research"
    kindToText Roadmap = "roadmap"
    kindToText Architecture = "architecture"
    kindToText ActivityLog = "activity_log"

-- | Format document data for display
formatDocData :: Value -> ProjectKnowledgeKind -> Text
formatDocData val kind = case kind of
  ProductResearch -> case fromJSON val of
    Success (prd :: ProductResearchData) -> T.unlines
      [ "Vision: " <> prdVision prd
      , "Value Proposition: " <> prdValueProposition prd
      , "Key Contacts: " <> T.pack (show $ length $ prdKeyContacts prd) <> " contacts"
      , "Open Questions: " <> T.pack (show $ length $ prdOpenQuestions prd) <> " questions"
      ]
    Error _ -> "(unable to parse)"
  Roadmap -> case fromJSON val of
    Success (rd :: RoadmapData) -> T.unlines
      [ "Milestones: " <> T.pack (show $ length $ rdMilestones rd)
      , "Timeline Notes: " <> rdTimelineNotes rd
      ]
    Error _ -> "(unable to parse)"
  Architecture -> case fromJSON val of
    Success (ad :: ArchitectureData) -> T.unlines
      [ "Users/Personas: " <> T.take 100 (adUsersPersonas ad)
      , "Testing: " <> T.take 100 (adTesting ad)
      , "Code Structure: " <> T.take 100 (adCodeStructure ad)
      ]
    Error _ -> "(unable to parse)"
  ActivityLog -> case fromJSON val of
    Success (ald :: ActivityLogData) -> T.unlines
      [ "Period: " <> aldPeriod ald
      , "Summary: " <> T.take 200 (aldSummary ald)
      , "Highlights: " <> T.pack (show $ length $ aldHighlights ald)
      ]
    Error _ -> "(unable to parse)"

-- | Format an activity for the prompt
formatActivity :: Activity -> Text
formatActivity act =
  let title = fromMaybe "(no title)" (activityTitle act)
      summary = fromMaybe "" (activitySummary act)
      source = T.pack $ show $ activitySource act
      sender = fromMaybe "" (activitySenderEmail act)
      actType = fromMaybe "" (activityType act)
      tags = activityTags act
      -- Build metadata parts
      metaParts = filter (not . T.null)
        [ if T.null sender then "" else "from: " <> sender
        , if T.null actType then "" else "type: " <> actType
        , if null tags then "" else "tags: " <> T.intercalate ", " tags
        ]
      metaStr = if null metaParts then "" else " [" <> T.intercalate "; " metaParts <> "]"
      summaryStr = if T.null summary then "" else "\n    " <> T.take 200 summary
  in "- [" <> source <> "] " <> title <> metaStr <> summaryStr

--------------------------------------------------------------------------------
-- Response Parsing and Persistence
--------------------------------------------------------------------------------

-- | LLM response for each document type
data LibrarianResponse = LibrarianResponse
  { lrRespProductResearch :: Either Text ProductResearchData
  , lrRespRoadmap :: Either Text RoadmapData
  , lrRespArchitecture :: Either Text ArchitectureData
  , lrRespActivityLog :: Either Text ActivityLogData
  } deriving (Show)

instance FromJSON LibrarianResponse where
  parseJSON = withObject "LibrarianResponse" $ \v -> LibrarianResponse
    <$> parseDocField v "product_research"
    <*> parseDocField v "roadmap"
    <*> parseDocField v "architecture"
    <*> parseDocField v "activity_log"
    where
      parseDocField :: FromJSON a => KM.KeyMap Value -> Text -> Parser (Either Text a)
      parseDocField obj key = do
        mVal <- obj .:? Key.fromText key
        case mVal of
          Nothing -> pure $ Left "no_change"
          Just (String "no_change") -> pure $ Left "no_change"
          Just val -> case fromJSON val of
            Success d -> pure $ Right d
            Error e -> pure $ Left $ T.pack e

-- | Parse LLM response and persist updates
-- Returns (updated, skipped, parseError)
parseAndPersistUpdates :: EntityId -> [Document] -> Text -> App ([ProjectKnowledgeKind], [ProjectKnowledgeKind], Text)
parseAndPersistUpdates projectId children respText = do
  -- Try to extract JSON from response (LLM might include markdown)
  let cleanedText = extractJson respText
  case eitherDecode (TLE.encodeUtf8 $ TL.fromStrict cleanedText) :: Either String LibrarianResponse of
    Left err -> do
      -- Show specific parse error
      pure ([], [ProductResearch, Roadmap, Architecture, ActivityLog], "JSON parse error: " <> T.pack err)
    Right resp -> do
      -- Process each document type
      prResult <- processUpdate projectId children ProductResearch (lrRespProductResearch resp)
      rmResult <- processUpdate projectId children Roadmap (lrRespRoadmap resp)
      arResult <- processUpdate projectId children Architecture (lrRespArchitecture resp)
      alResult <- processUpdate projectId children ActivityLog (lrRespActivityLog resp)

      let results = [(ProductResearch, prResult), (Roadmap, rmResult), (Architecture, arResult), (ActivityLog, alResult)]
      let updated = [k | (k, True) <- results]
      let skipped = [k | (k, False) <- results]
      pure (updated, skipped, "")

-- | Extract JSON from response (handles markdown code blocks)
extractJson :: Text -> Text
extractJson txt
  | "```json" `T.isInfixOf` txt =
      let afterStart = T.drop 1 $ T.dropWhile (/= '\n') $ snd $ T.breakOn "```json" txt
          beforeEnd = fst $ T.breakOn "```" afterStart
      in T.strip beforeEnd
  | "```" `T.isInfixOf` txt =
      let afterStart = T.drop 1 $ T.dropWhile (/= '\n') $ snd $ T.breakOn "```" txt
          beforeEnd = fst $ T.breakOn "```" afterStart
      in T.strip beforeEnd
  | otherwise = T.strip txt

-- | Process a single document type update
processUpdate :: ToJSON a => EntityId -> [Document] -> ProjectKnowledgeKind -> Either Text a -> App Bool
processUpdate projectId children kind eUpdate = case eUpdate of
  Left _ -> pure False  -- no_change or error
  Right newData -> do
    now <- liftIO getCurrentTime
    let dataWithKind = addKindAndTimestamp kind now newData
    case findChildByKind children kind of
      Nothing -> do
        -- Create new document
        let newDoc = NewDocument
              { newDocTenantId = Nothing
              , newDocType = ProjectKnowledgeDoc
              , newDocData = dataWithKind
              , newDocTags = [kindToTag kind]
              , newDocConfidence = Just 0.8
              , newDocSource = Just "librarian"
              , newDocSupersedesId = Nothing
              , newDocParentId = Just projectId
              }
        _ <- insertDocument newDoc
        pure True
      Just existingDoc -> do
        -- Supersede existing document
        let newDoc = NewDocument
              { newDocTenantId = documentTenantId existingDoc
              , newDocType = ProjectKnowledgeDoc
              , newDocData = dataWithKind
              , newDocTags = documentTags existingDoc
              , newDocConfidence = Just 0.8
              , newDocSource = Just "librarian"
              , newDocSupersedesId = Nothing
              , newDocParentId = Just projectId
              }
        _ <- insertWithSupersedes newDoc (documentId existingDoc)
        pure True

-- | Add kind field and timestamp to document data
addKindAndTimestamp :: ToJSON a => ProjectKnowledgeKind -> UTCTime -> a -> Value
addKindAndTimestamp kind now d = case toJSON d of
  Object obj -> Object $ KM.insert (Key.fromText "kind") (toJSON kind)
                       $ KM.insert (Key.fromText "updated_at") (toJSON now) obj
  v -> v

-- | Convert kind to tag
kindToTag :: ProjectKnowledgeKind -> Text
kindToTag ProductResearch = "knowledge:product_research"
kindToTag Roadmap = "knowledge:roadmap"
kindToTag Architecture = "knowledge:architecture"
kindToTag ActivityLog = "knowledge:activity_log"
