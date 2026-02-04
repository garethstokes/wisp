module Time
  ( loadTimezone
  , utcToLocal
  , formatLocalTime
  , TZ
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, LocalTime)
import Data.Time.Zones (TZ, loadSystemTZ, utcToLocalTimeTZ)
import Control.Exception (try, SomeException)

-- Load timezone from IANA name, fail with helpful error if invalid
loadTimezone :: Text -> IO TZ
loadTimezone tzName = do
  result <- try $ loadSystemTZ (T.unpack tzName)
  case result of
    Left (_ :: SomeException) -> error $ unlines
      [ "Invalid timezone '" <> T.unpack tzName <> "'."
      , ""
      , "Use IANA format like 'Europe/London' or 'America/New_York'"
      , ""
      , "Common timezones:"
      , "  Europe/London, Europe/Paris, Europe/Berlin"
      , "  America/New_York, America/Los_Angeles, America/Chicago"
      , "  Asia/Tokyo, Asia/Shanghai, Asia/Singapore"
      , "  Australia/Sydney, Pacific/Auckland"
      ]
    Right tz -> pure tz

-- Convert UTC to local time in the given timezone
utcToLocal :: TZ -> UTCTime -> LocalTime
utcToLocal = utcToLocalTimeTZ

-- Format a LocalTime for display (simple ISO-ish format without timezone suffix)
formatLocalTime :: LocalTime -> Text
formatLocalTime lt = T.pack $ show lt
