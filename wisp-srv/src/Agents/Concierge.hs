module Agents.Concierge
  ( classifyAllPending
  , classifyPending
  ) where

import Control.Monad (forM, void)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import App.Monad (App)
import Domain.Activity (Activity(..), ActivityStatus)
import qualified Domain.Activity as Activity
import Domain.Classification (Classification(..))
import Domain.Person (Person(..))
import Domain.Receipt (NewReceipt(..), ReceiptAction(..))
import Infra.Db.Activity (getActivitiesByStatus, updateActivityClassification)
import Infra.Db.Receipt (insertReceipt)
import Agents.Concierge.Classifier (classifyActivity)
import Services.PeopleResolver (resolvePersonForActivity)
import Services.Router (routeActivity)

-- Process all pending activities through the pipeline
classifyAllPending :: Int -> App [(Text, Either Text ActivityStatus)]
classifyAllPending limit = do
  activities <- getActivitiesByStatus Activity.Pending limit
  forM activities $ \activity -> do
    result <- classifyPending activity
    let actId = T.pack $ show (activityId activity)
    pure (actId, result)

-- Process a single activity through classify -> resolve -> route
classifyPending :: Activity -> App (Either Text ActivityStatus)
classifyPending activity = do
  -- Step 1: Classify
  liftIO $ putStrLn $ "Classifying activity: " <> show (activityId activity)
  classifyResult <- classifyActivity activity
  case classifyResult of
    Left err -> do
      liftIO $ putStrLn $ "Classification failed: " <> T.unpack err
      -- Create failure receipt
      void $ insertReceipt NewReceipt
        { newReceiptActivityId = activityId activity
        , newReceiptActionTaken = ClassificationFailed
        , newReceiptActionDetail = Just err
        , newReceiptConfidence = Nothing
        }
      pure $ Left err
    Right classification -> do
      liftIO $ putStrLn $ "Classification: tier=" <> show (classificationAutonomyTier classification)
                       <> ", confidence=" <> show (classificationConfidence classification)

      -- Create classification receipt
      void $ insertReceipt NewReceipt
        { newReceiptActivityId = activityId activity
        , newReceiptActionTaken = Classified
        , newReceiptActionDetail = Just (classificationReasoning classification)
        , newReceiptConfidence = Just (classificationConfidence classification)
        }

      -- Step 2: Resolve person
      mPerson <- resolvePersonForActivity activity
      let mPersonId = fmap personId mPerson
      liftIO $ putStrLn $ "Resolved person: " <> show (fmap personEmail mPerson)

      -- Step 3: Update activity with classification
      updateActivityClassification (activityId activity) classification mPersonId

      -- Step 4: Route
      newStatus <- routeActivity activity classification
      liftIO $ putStrLn $ "Routed to status: " <> show newStatus

      -- Create routing receipt
      void $ insertReceipt NewReceipt
        { newReceiptActivityId = activityId activity
        , newReceiptActionTaken = statusToReceiptAction newStatus
        , newReceiptActionDetail = Nothing
        , newReceiptConfidence = Nothing
        }

      pure $ Right newStatus

-- Convert ActivityStatus to appropriate ReceiptAction
statusToReceiptAction :: ActivityStatus -> ReceiptAction
statusToReceiptAction Activity.Pending = Classified  -- shouldn't happen
statusToReceiptAction Activity.NeedsReview = RoutedToNeedsReview
statusToReceiptAction Activity.Quarantined = RoutedToQuarantined
statusToReceiptAction Activity.Surfaced = RoutedToSurfaced
statusToReceiptAction Activity.Processed = RoutedToProcessed
statusToReceiptAction Activity.Archived = RoutedToArchived
