module Services.Router
  ( routeActivity
  , determineStatus
  ) where

import Data.Text (Text)
import App.Monad (App, getConfig)
import App.Config (Config(..), ClassificationConfig(..))
import Domain.Activity (Activity(..), ActivityStatus(..))
import qualified Domain.Activity as Act
import Domain.Classification (Classification(..))
import Domain.Receipt (NewReceipt(..), ReceiptAction(..))
import qualified Domain.Receipt as Rcpt
import Infra.Db.Activity (updateActivityStatus)
import Infra.Db.Receipt (insertReceipt)

-- Route an activity based on its classification
routeActivity :: Activity -> Classification -> App ActivityStatus
routeActivity activity classification = do
  cfg <- getConfig
  let threshold = cfg.classification.confidenceThreshold
  let newStatus = determineStatus threshold classification

  -- Update activity status
  updateActivityStatus (activityId activity) newStatus

  -- Log receipt
  let action = statusToAction newStatus
  let detail = buildActionDetail classification newStatus
  _ <- insertReceipt NewReceipt
    { newReceiptActivityId = activityId activity
    , newReceiptActionTaken = action
    , newReceiptActionDetail = Just detail
    , newReceiptConfidence = Just (classificationConfidence classification)
    }

  pure newStatus

-- Determine status based on confidence and tier
determineStatus :: Double -> Classification -> ActivityStatus
determineStatus threshold classification
  | classificationConfidence classification < threshold = Act.Quarantined
  | classificationAutonomyTier classification == 1 = Act.Processed
  | classificationAutonomyTier classification == 2 = Act.Processed
  | classificationAutonomyTier classification == 3 = Pending
  | classificationAutonomyTier classification >= 4 = Act.Surfaced
  | otherwise = Pending

-- Map status to receipt action
statusToAction :: ActivityStatus -> ReceiptAction
statusToAction Act.Quarantined = Rcpt.Quarantined
statusToAction Act.Processed = Rcpt.Processed
statusToAction Act.Surfaced = Rcpt.Surfaced
statusToAction _ = Classified

-- Build human-readable action detail
buildActionDetail :: Classification -> ActivityStatus -> Text
buildActionDetail c status =
  let tierDesc = case classificationAutonomyTier c of
        1 -> "Tier 1: Silent processing"
        2 -> "Tier 2: Noted"
        3 -> "Tier 3: Needs review"
        4 -> "Tier 4: Surfaced for attention"
        _ -> "Unknown tier"
      statusDesc = case status of
        Act.Quarantined -> "Low confidence, quarantined for review"
        Act.Processed -> "Processed automatically"
        Act.Surfaced -> "Surfaced for user attention"
        Pending -> "Awaiting review"
        Archived -> "Archived"
  in tierDesc <> ". " <> statusDesc <> ". Summary: " <> classificationSummary c
