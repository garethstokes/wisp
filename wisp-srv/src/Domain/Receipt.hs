-- src/Domain/Receipt.hs
module Domain.Receipt
  ( Receipt(..)
  , ReceiptAction(..)
  , NewReceipt(..)
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), withText)
import Data.Text (Text)
import Data.Time (UTCTime)
import Domain.Id (EntityId)

data ReceiptAction
  = Classified
  | Quarantined
  | Processed
  | Surfaced
  deriving (Eq, Show)

instance ToJSON ReceiptAction where
  toJSON Classified = "classified"
  toJSON Quarantined = "quarantined"
  toJSON Processed = "processed"
  toJSON Surfaced = "surfaced"

instance FromJSON ReceiptAction where
  parseJSON = withText "ReceiptAction" $ \case
    "classified" -> pure Classified
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

data NewReceipt = NewReceipt
  { newReceiptActivityId :: EntityId
  , newReceiptActionTaken :: ReceiptAction
  , newReceiptActionDetail :: Maybe Text
  , newReceiptConfidence :: Maybe Double
  } deriving (Show)
