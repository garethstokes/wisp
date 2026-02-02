module Services.Pipeline
  ( processPendingActivities
  , processActivity
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import App.Monad (App)
import Domain.Activity (Activity(..), ActivityStatus(..))
import Domain.Classification (Classification(..))
import Domain.Person (Person(..))
import Infra.Db.Activity (getActivitiesByStatus, updateActivityClassification)
import Services.Classifier (classifyActivity)
import Services.PeopleResolver (resolvePersonForActivity)
import Services.Router (routeActivity)

-- Process all pending activities through the pipeline
processPendingActivities :: Int -> App [(Text, Either Text ActivityStatus)]
processPendingActivities limit = do
  activities <- getActivitiesByStatus Pending limit
  forM activities $ \activity -> do
    result <- processActivity activity
    let actId = T.pack $ show (activityId activity)
    pure (actId, result)

-- Process a single activity through classify -> resolve -> route
processActivity :: Activity -> App (Either Text ActivityStatus)
processActivity activity = do
  -- Step 1: Classify
  liftIO $ putStrLn $ "Classifying activity: " <> show (activityId activity)
  classifyResult <- classifyActivity activity
  case classifyResult of
    Left err -> do
      liftIO $ putStrLn $ "Classification failed: " <> T.unpack err
      pure $ Left err
    Right classification -> do
      liftIO $ putStrLn $ "Classification: tier=" <> show (classificationAutonomyTier classification)
                       <> ", confidence=" <> show (classificationConfidence classification)

      -- Step 2: Resolve person
      mPerson <- resolvePersonForActivity activity
      let mPersonId = fmap personId mPerson
      liftIO $ putStrLn $ "Resolved person: " <> show (fmap personEmail mPerson)

      -- Step 3: Update activity with classification
      updateActivityClassification (activityId activity) classification mPersonId

      -- Step 4: Route
      newStatus <- routeActivity activity classification
      liftIO $ putStrLn $ "Routed to status: " <> show newStatus

      pure $ Right newStatus
