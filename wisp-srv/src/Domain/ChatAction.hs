module Domain.ChatAction
  ( ChatAction(..)
  , ActionFilter(..)
  , ActionResult(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), withObject, (.:), withText)
import Data.Text (Text)
import GHC.Generics (Generic)

-- Filter for bulk operations
data ActionFilter
  = AllSurfaced
  | AllQuarantined
  | AllNeedsReview
  | ByIds [Text]  -- Specific activity IDs
  deriving (Show, Eq, Generic)

instance FromJSON ActionFilter where
  parseJSON = withText "ActionFilter" $ \case
    "all_surfaced" -> pure AllSurfaced
    "all_quarantined" -> pure AllQuarantined
    "all_needs_review" -> pure AllNeedsReview
    _ -> fail "Invalid filter"

instance ToJSON ActionFilter where
  toJSON AllSurfaced = "all_surfaced"
  toJSON AllQuarantined = "all_quarantined"
  toJSON AllNeedsReview = "all_needs_review"
  toJSON (ByIds _) = "by_ids"

-- Actions the chat can request
data ChatAction
  = MarkComplete ActionFilter           -- Mark activities as processed/complete
  | ApproveActivity Text                 -- Approve a quarantined item (by ID)
  | DismissActivity Text                 -- Dismiss/archive an activity (by ID)
  | NoAction Text                        -- Just respond with text, no action needed
  deriving (Show, Eq, Generic)

instance FromJSON ChatAction where
  parseJSON = withObject "ChatAction" $ \v -> do
    action <- v .: "action"
    case (action :: Text) of
      "mark_complete" -> do
        filterVal <- v .: "filter"
        case (filterVal :: Text) of
          "all_surfaced" -> pure $ MarkComplete AllSurfaced
          "all_quarantined" -> pure $ MarkComplete AllQuarantined
          "all_needs_review" -> pure $ MarkComplete AllNeedsReview
          "by_ids" -> do
            ids <- v .: "ids"
            pure $ MarkComplete (ByIds ids)
          _ -> fail "Invalid filter for mark_complete"
      "approve" -> ApproveActivity <$> v .: "id"
      "dismiss" -> DismissActivity <$> v .: "id"
      "none" -> NoAction <$> v .: "response"
      _ -> fail "Unknown action type"

-- Result of executing an action
data ActionResult
  = ActionSuccess Text Int    -- Message and count of affected items
  | ActionError Text          -- Error message
  deriving (Show, Eq, Generic)
