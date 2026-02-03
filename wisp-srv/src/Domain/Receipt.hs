-- src/Domain/Receipt.hs
module Domain.Receipt
  ( Receipt(..)
  , ReceiptAction(..)
  , NewReceipt(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=), withText)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId(..))

data ReceiptAction
  = Classified
  | ClassificationFailed
  | RoutedToQuarantined
  | RoutedToProcessed
  | RoutedToSurfaced
  | RoutedToNeedsReview
  | RoutedToArchived
  -- Legacy (for backwards compat)
  | Quarantined
  | Processed
  | Surfaced
  deriving (Eq, Show)

instance ToJSON ReceiptAction where
  toJSON Classified = "classified"
  toJSON ClassificationFailed = "classification_failed"
  toJSON RoutedToQuarantined = "routed_to_quarantined"
  toJSON RoutedToProcessed = "routed_to_processed"
  toJSON RoutedToSurfaced = "routed_to_surfaced"
  toJSON RoutedToNeedsReview = "routed_to_needs_review"
  toJSON RoutedToArchived = "routed_to_archived"
  toJSON Quarantined = "quarantined"
  toJSON Processed = "processed"
  toJSON Surfaced = "surfaced"

instance FromJSON ReceiptAction where
  parseJSON = withText "ReceiptAction" $ \case
    "classified" -> pure Classified
    "classification_failed" -> pure ClassificationFailed
    "routed_to_quarantined" -> pure RoutedToQuarantined
    "routed_to_processed" -> pure RoutedToProcessed
    "routed_to_surfaced" -> pure RoutedToSurfaced
    "routed_to_needs_review" -> pure RoutedToNeedsReview
    "routed_to_archived" -> pure RoutedToArchived
    "quarantined" -> pure Quarantined
    "processed" -> pure Processed
    "surfaced" -> pure Surfaced
    other -> fail $ "Invalid receipt action: " <> show other

data Receipt = Receipt
  { receiptId :: EntityId
  , receiptActivityId :: EntityId
  , receiptActionTaken :: ReceiptAction
  , receiptActionDetail :: Maybe Text
  , receiptConfidence :: Maybe Double
  , receiptCreatedAt :: UTCTime
  } deriving (Show)

instance ToJSON Receipt where
  toJSON r = object
    [ "id" .= unEntityId (receiptId r)
    , "activity_id" .= unEntityId (receiptActivityId r)
    , "action_taken" .= receiptActionTaken r
    , "action_detail" .= receiptActionDetail r
    , "confidence" .= receiptConfidence r
    , "created_at" .= receiptCreatedAt r
    ]

data NewReceipt = NewReceipt
  { newReceiptActivityId :: EntityId
  , newReceiptActionTaken :: ReceiptAction
  , newReceiptActionDetail :: Maybe Text
  , newReceiptConfidence :: Maybe Double
  } deriving (Show)
