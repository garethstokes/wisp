-- src/Infra/Db/Receipt.hs
module Infra.Db.Receipt
  ( insertReceipt
  , getReceiptsForActivity
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Domain.Id (EntityId(..), newEntityId)
import Domain.Receipt (Receipt(..), NewReceipt(..), ReceiptAction(..))
import App.Monad (App, getConn)

instance FromRow Receipt where
  fromRow = Receipt
    <$> (EntityId <$> field)          -- id
    <*> (EntityId <$> field)          -- activity_id
    <*> (parseAction <$> field)       -- action_taken
    <*> field                          -- action_detail
    <*> field                          -- confidence
    <*> field                          -- created_at
    where
      parseAction :: Text -> ReceiptAction
      parseAction "classified" = Classified
      parseAction "classification_failed" = ClassificationFailed
      parseAction "routed_to_quarantined" = RoutedToQuarantined
      parseAction "routed_to_processed" = RoutedToProcessed
      parseAction "routed_to_surfaced" = RoutedToSurfaced
      parseAction "routed_to_needs_review" = RoutedToNeedsReview
      parseAction "routed_to_archived" = RoutedToArchived
      parseAction "quarantined" = Quarantined
      parseAction "processed" = Processed
      parseAction "surfaced" = Surfaced
      parseAction _ = Classified  -- default

-- Insert a new receipt
insertReceipt :: NewReceipt -> App EntityId
insertReceipt new = do
  conn <- getConn
  rid <- liftIO newEntityId
  let actionText = case newReceiptActionTaken new of
        Classified -> "classified" :: Text
        ClassificationFailed -> "classification_failed"
        RoutedToQuarantined -> "routed_to_quarantined"
        RoutedToProcessed -> "routed_to_processed"
        RoutedToSurfaced -> "routed_to_surfaced"
        RoutedToNeedsReview -> "routed_to_needs_review"
        RoutedToArchived -> "routed_to_archived"
        Quarantined -> "quarantined"
        Processed -> "processed"
        Surfaced -> "surfaced"
  _ <- liftIO $ execute conn
    "insert into receipts (id, activity_id, action_taken, action_detail, confidence) \
    \values (?, ?, ?, ?, ?)"
    ( unEntityId rid
    , unEntityId (newReceiptActivityId new)
    , actionText
    , newReceiptActionDetail new
    , newReceiptConfidence new
    )
  pure rid

-- Get all receipts for an activity
getReceiptsForActivity :: EntityId -> App [Receipt]
getReceiptsForActivity actId = do
  conn <- getConn
  liftIO $ query conn
    "select id, activity_id, action_taken, action_detail, confidence, created_at \
    \from receipts where activity_id = ? order by created_at"
    (Only $ unEntityId actId)
