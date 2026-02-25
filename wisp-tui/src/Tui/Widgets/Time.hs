module Tui.Widgets.Time
  ( relativeTime
  , humanDate
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, diffUTCTime, NominalDiffTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- | Format a time relative to now (e.g., "2 hours ago", "yesterday")
relativeTime :: UTCTime -> UTCTime -> Text
relativeTime now t
  | diff < 60 = "just now"
  | diff < 3600 = showMinutes
  | diff < 86400 = showHours
  | diff < 172800 = "yesterday"
  | diff < 604800 = showDays
  | otherwise = humanDate t
  where
    diff :: NominalDiffTime
    diff = diffUTCTime now t

    diffSecs :: Int
    diffSecs = round diff

    showMinutes = let mins = diffSecs `div` 60
                  in if mins == 1 then "1 minute ago" else T.pack (show mins) <> " minutes ago"

    showHours = let hrs = diffSecs `div` 3600
                in if hrs == 1 then "1 hour ago" else T.pack (show hrs) <> " hours ago"

    showDays = let days = diffSecs `div` 86400
               in if days == 1 then "1 day ago" else T.pack (show days) <> " days ago"

-- | Format a date for human reading (e.g., "Feb 23, 2026")
humanDate :: UTCTime -> Text
humanDate = T.pack . formatTime defaultTimeLocale "%b %d, %Y"
