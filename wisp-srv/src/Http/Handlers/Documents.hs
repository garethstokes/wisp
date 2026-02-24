-- src/Http/Handlers/Documents.hs
module Http.Handlers.Documents
  ( getProjectsList
  , postProject
  , postProjectArchive
  , postProjectReflect
  , postProjectLibrarian
  , postProjectLibrarianByTag
  , getProjectSuggestions
  , getProjectChildrenHandler
  , getNotesList
  , postNote
  , getPrefsList
  , postPref
  , getDocumentById
  , getDocumentLogHandler
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (FromJSON(..), ToJSON(..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Network.HTTP.Types.Status (status201, status404)
import Web.Scotty.Trans (ActionT, json, jsonData, captureParam, status)
import App.Monad (Env, runApp)
import Domain.Id (EntityId(..), unEntityId)
import Domain.Document
import qualified Data.Aeson as Aeson
import qualified Infra.Db.Document as Db
import Skills.Reflection (runProjectReflection, ReflectionResult(..), detectProjectClusters, ProjectSuggestion(..), ClusterKey(..))
import qualified Skills.Librarian as Librarian

--------------------------------------------------------------------------------
-- Request types
--------------------------------------------------------------------------------

-- | Request body for creating a project
data CreateProjectRequest = CreateProjectRequest
  { createProjectName :: Text
  , createProjectType :: Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreateProjectRequest where
  parseJSON = withObject "CreateProjectRequest" $ \v ->
    CreateProjectRequest <$> v .: "name" <*> v .: "type"

instance ToJSON CreateProjectRequest where
  toJSON req = object ["name" .= createProjectName req, "type" .= createProjectType req]

-- | Request body for creating a note
data CreateNoteRequest = CreateNoteRequest
  { createNoteTitle :: Text
  , createNoteContent :: Maybe Text
  , createNoteTags :: Maybe [Text]
  } deriving (Show, Eq, Generic)

instance FromJSON CreateNoteRequest where
  parseJSON = withObject "CreateNoteRequest" $ \v ->
    CreateNoteRequest <$> v .: "title" <*> v .:? "content" <*> v .:? "tags"

instance ToJSON CreateNoteRequest where
  toJSON req = object
    [ "title" .= createNoteTitle req
    , "content" .= createNoteContent req
    , "tags" .= createNoteTags req
    ]

-- | Request body for creating a preference
data CreatePrefRequest = CreatePrefRequest
  { createPrefKey :: Text
  , createPrefValue :: Text
  , createPrefContext :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON CreatePrefRequest where
  parseJSON = withObject "CreatePrefRequest" $ \v ->
    CreatePrefRequest <$> v .: "key" <*> v .: "value" <*> v .:? "context"

instance ToJSON CreatePrefRequest where
  toJSON req = object
    [ "key" .= createPrefKey req
    , "value" .= createPrefValue req
    , "context" .= createPrefContext req
    ]

--------------------------------------------------------------------------------
-- Project Endpoints
--------------------------------------------------------------------------------

-- | GET /api/projects - list active projects
getProjectsList :: ActionT (ReaderT Env IO) ()
getProjectsList = do
  projects <- lift $ Db.getDocumentsByType ProjectDoc True 100
  json $ object ["projects" .= projects, "count" .= length projects]

-- | POST /api/projects - create a project
postProject :: ActionT (ReaderT Env IO) ()
postProject = do
  req <- jsonData :: ActionT (ReaderT Env IO) CreateProjectRequest
  let projData = Aeson.object
        [ "name" .= createProjectName req
        , "type" .= createProjectType req
        ]
  let newDoc = NewDocument
        { newDocTenantId = Nothing
        , newDocType = ProjectDoc
        , newDocData = projData
        , newDocTags = []
        , newDocConfidence = Nothing
        , newDocSource = Just "user"
        , newDocSupersedesId = Nothing
        , newDocParentId = Nothing
        }
  docId <- lift $ Db.insertDocument newDoc
  let logEntry = NewDocumentLogEntry
        { newLogDocumentId = docId
        , newLogActivityId = Nothing
        , newLogDescription = Just "Created project"
        , newLogSource = LogUser
        , newLogAgentId = Nothing
        }
  _ <- lift $ Db.insertDocumentLog logEntry
  status status201
  json $ object ["id" .= docId, "status" .= ("created" :: Text)]

-- | POST /api/projects/:id/archive - archive a project
postProjectArchive :: ActionT (ReaderT Env IO) ()
postProjectArchive = do
  docId <- captureParam "id"
  lift $ Db.archiveDocument (EntityId docId)
  json $ object ["status" .= ("archived" :: Text)]

-- | POST /api/projects/reflect - run reflection on all projects
postProjectReflect :: ActionT (ReaderT Env IO) ()
postProjectReflect = do
  results <- lift runProjectReflection
  let response = object
        [ "status" .= ("completed" :: Text)
        , "projects_updated" .= length results
        , "results" .= map resultToJson results
        ]
  json response
  where
    resultToJson :: ReflectionResult -> Aeson.Value
    resultToJson r = object
      [ "project_id" .= unEntityId (rrProjectId r)
      , "summary" .= rrUpdatedSummary r
      , "status" .= rrUpdatedStatus r
      , "activities_processed" .= rrActivitiesProcessed r
      ]

-- | POST /api/projects/librarian - run librarian on all projects
postProjectLibrarian :: ActionT (ReaderT Env IO) ()
postProjectLibrarian = do
  results <- lift Librarian.runLibrarian
  let response = object
        [ "status" .= ("completed" :: Text)
        , "projects_processed" .= length results
        , "results" .= map librarianResultToJson results
        ]
  json response

-- | POST /api/projects/librarian/:tag - run librarian for a single project by tag
postProjectLibrarianByTag :: ActionT (ReaderT Env IO) ()
postProjectLibrarianByTag = do
  tag <- captureParam "tag"
  mDoc <- lift $ Db.getProjectByTag tag
  case mDoc of
    Nothing -> do
      status status404
      json $ object ["error" .= ("Project not found" :: Text)]
    Just doc -> do
      mResult <- lift $ Librarian.runLibrarianForProject doc
      case mResult of
        Nothing -> json $ object
          [ "status" .= ("skipped" :: Text)
          , "message" .= ("No activities or unable to process" :: Text)
          ]
        Just result -> json $ object
          [ "status" .= ("completed" :: Text)
          , "result" .= librarianResultToJson result
          ]

-- | Convert LibrarianResult to JSON
librarianResultToJson :: Librarian.LibrarianResult -> Aeson.Value
librarianResultToJson r = object
  [ "project_id" .= unEntityId (Librarian.lrProjectId r)
  , "project_name" .= Librarian.lrProjectName r
  , "updated" .= map show (Librarian.lrUpdatedDocs r)
  , "skipped" .= map show (Librarian.lrSkippedDocs r)
  ]

-- | GET /api/projects/suggestions - get new project suggestions based on activity clusters
getProjectSuggestions :: ActionT (ReaderT Env IO) ()
getProjectSuggestions = do
  suggestions <- lift detectProjectClusters
  let response = object
        [ "suggestions" .= map suggestionToJson suggestions
        , "count" .= length suggestions
        ]
  json response
  where
    suggestionToJson :: ProjectSuggestion -> Aeson.Value
    suggestionToJson s = object
      [ "suggested_name" .= psSuggestedName s
      , "cluster_key" .= unClusterKey (psClusterKey s)
      , "reason" .= psReason s
      , "sample_activity_ids" .= map unEntityId (psSampleActivityIds s)
      , "activity_count" .= psActivityCount s
      ]

-- | GET /api/projects/:id/children - get child documents for a project
getProjectChildrenHandler :: ActionT (ReaderT Env IO) ()
getProjectChildrenHandler = do
  docId <- captureParam "id"
  children <- lift $ Db.getProjectChildren (EntityId docId)
  json $ object ["children" .= children, "count" .= length children]

--------------------------------------------------------------------------------
-- Note Endpoints
--------------------------------------------------------------------------------

-- | GET /api/notes - list notes
getNotesList :: ActionT (ReaderT Env IO) ()
getNotesList = do
  notes <- lift $ Db.getDocumentsByType NoteDoc True 100
  json $ object ["notes" .= notes, "count" .= length notes]

-- | POST /api/notes - create a note
postNote :: ActionT (ReaderT Env IO) ()
postNote = do
  req <- jsonData :: ActionT (ReaderT Env IO) CreateNoteRequest
  let noteData = Aeson.object
        [ "title" .= createNoteTitle req
        , "content" .= createNoteContent req
        ]
  let tags = maybe [] id (createNoteTags req)
  let newDoc = NewDocument
        { newDocTenantId = Nothing
        , newDocType = NoteDoc
        , newDocData = noteData
        , newDocTags = tags
        , newDocConfidence = Nothing
        , newDocSource = Just "user"
        , newDocSupersedesId = Nothing
        , newDocParentId = Nothing
        }
  docId <- lift $ Db.insertDocument newDoc
  let logEntry = NewDocumentLogEntry
        { newLogDocumentId = docId
        , newLogActivityId = Nothing
        , newLogDescription = Just "Created note"
        , newLogSource = LogUser
        , newLogAgentId = Nothing
        }
  _ <- lift $ Db.insertDocumentLog logEntry
  status status201
  json $ object ["id" .= docId, "status" .= ("created" :: Text)]

--------------------------------------------------------------------------------
-- Preference Endpoints
--------------------------------------------------------------------------------

-- | GET /api/preferences - list preferences
getPrefsList :: ActionT (ReaderT Env IO) ()
getPrefsList = do
  prefs <- lift $ Db.getDocumentsByType PreferenceDoc True 100
  json $ object ["preferences" .= prefs, "count" .= length prefs]

-- | POST /api/preferences - set a preference
postPref :: ActionT (ReaderT Env IO) ()
postPref = do
  req <- jsonData :: ActionT (ReaderT Env IO) CreatePrefRequest
  let prefData = Aeson.object
        [ "key" .= createPrefKey req
        , "value" .= createPrefValue req
        , "context" .= createPrefContext req
        ]
  let newDoc = NewDocument
        { newDocTenantId = Nothing
        , newDocType = PreferenceDoc
        , newDocData = prefData
        , newDocTags = []
        , newDocConfidence = Nothing
        , newDocSource = Just "user"
        , newDocSupersedesId = Nothing
        , newDocParentId = Nothing
        }
  docId <- lift $ Db.insertDocument newDoc
  let logEntry = NewDocumentLogEntry
        { newLogDocumentId = docId
        , newLogActivityId = Nothing
        , newLogDescription = Just "Set preference"
        , newLogSource = LogUser
        , newLogAgentId = Nothing
        }
  _ <- lift $ Db.insertDocumentLog logEntry
  status status201
  json $ object ["id" .= docId, "status" .= ("created" :: Text)]

--------------------------------------------------------------------------------
-- Generic Document Endpoints
--------------------------------------------------------------------------------

-- | GET /api/documents/:id - get document by ID
getDocumentById :: ActionT (ReaderT Env IO) ()
getDocumentById = do
  docId <- captureParam "id"
  mDoc <- lift $ Db.getDocumentById (EntityId docId)
  case mDoc of
    Just doc -> json doc
    Nothing -> do
      status status404
      json $ object ["error" .= ("Document not found" :: Text)]

-- | GET /api/documents/:id/log - get document log
getDocumentLogHandler :: ActionT (ReaderT Env IO) ()
getDocumentLogHandler = do
  docId <- captureParam "id"
  entries <- lift $ Db.getDocumentLog (EntityId docId) 50
  json $ object ["log" .= entries, "count" .= length entries]
