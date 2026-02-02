module Infra.Google.Auth
  ( OAuthConfig(..)
  , TokenResponse(..)
  , UserInfo(..)
  , buildAuthUrl
  , exchangeCode
  , refreshAccessToken
  , getUserInfo
  ) where

import Data.Aeson (FromJSON(..), (.:), (.:?), withObject)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)

data OAuthConfig = OAuthConfig
  { oauthClientId :: Text
  , oauthClientSecret :: Text
  , oauthRedirectUri :: Text
  } deriving (Show)

data TokenResponse = TokenResponse
  { accessToken :: Text
  , refreshToken :: Maybe Text
  , expiresIn :: Int
  , tokenType :: Text
  } deriving (Show)

instance FromJSON TokenResponse where
  parseJSON = withObject "TokenResponse" $ \v -> TokenResponse
    <$> v .: "access_token"
    <*> v .:? "refresh_token"
    <*> v .: "expires_in"
    <*> v .: "token_type"

data UserInfo = UserInfo
  { userEmail :: Text
  , userName :: Maybe Text
  , userPicture :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON UserInfo where
  parseJSON = withObject "UserInfo" $ \v -> UserInfo
    <$> v .: "email"
    <*> v .:? "name"
    <*> v .:? "picture"

-- URL-encode a text value
urlEncode :: Text -> Text
urlEncode = T.pack . concatMap encodeChar . T.unpack
  where
    encodeChar c
      | c >= 'a' && c <= 'z' = [c]
      | c >= 'A' && c <= 'Z' = [c]
      | c >= '0' && c <= '9' = [c]
      | c `elem` ['-', '_', '.', '~'] = [c]
      | otherwise = '%' : showHex2 (fromEnum c)
    showHex2 n = let (q, r) = n `divMod` 16
                 in [hexDigit q, hexDigit r]
    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + n)
      | otherwise = toEnum (fromEnum 'A' + n - 10)

-- Build the OAuth authorization URL
buildAuthUrl :: OAuthConfig -> Text
buildAuthUrl cfg = T.concat
  [ "https://accounts.google.com/o/oauth2/v2/auth"
  , "?client_id=", urlEncode (oauthClientId cfg)
  , "&redirect_uri=", urlEncode (oauthRedirectUri cfg)
  , "&response_type=code"
  , "&scope=", urlEncode scopes
  , "&access_type=offline"
  , "&prompt=consent"
  ]
  where
    scopes = "https://www.googleapis.com/auth/gmail.readonly https://www.googleapis.com/auth/calendar.readonly"

-- Exchange authorization code for tokens
exchangeCode :: OAuthConfig -> Text -> IO (Either Text TokenResponse)
exchangeCode cfg code = do
  manager <- newManager tlsManagerSettings
  let body = BS.intercalate "&"
        [ "client_id=" <> encodeUtf8 (oauthClientId cfg)
        , "client_secret=" <> encodeUtf8 (oauthClientSecret cfg)
        , "code=" <> encodeUtf8 code
        , "redirect_uri=" <> encodeUtf8 (oauthRedirectUri cfg)
        , "grant_type=authorization_code"
        ]
  initialReq <- parseRequest "https://oauth2.googleapis.com/token"
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
        , requestBody = RequestBodyBS body
        }
  response <- httpLbs req manager
  pure $ case Aeson.decode (responseBody response) of
    Just tok -> Right tok
    Nothing -> Left $ "Failed to parse token response: " <> T.pack (show $ responseBody response)

-- Refresh an access token
refreshAccessToken :: OAuthConfig -> Text -> IO (Either Text TokenResponse)
refreshAccessToken cfg refreshTok = do
  manager <- newManager tlsManagerSettings
  let body = BS.intercalate "&"
        [ "client_id=" <> encodeUtf8 (oauthClientId cfg)
        , "client_secret=" <> encodeUtf8 (oauthClientSecret cfg)
        , "refresh_token=" <> encodeUtf8 refreshTok
        , "grant_type=refresh_token"
        ]
  initialReq <- parseRequest "https://oauth2.googleapis.com/token"
  let req = initialReq
        { method = "POST"
        , requestHeaders = [("Content-Type", "application/x-www-form-urlencoded")]
        , requestBody = RequestBodyBS body
        }
  response <- httpLbs req manager
  pure $ case Aeson.decode (responseBody response) of
    Just tok -> Right tok
    Nothing -> Left "Failed to parse refresh token response"

-- Fetch user info using access token
getUserInfo :: Text -> IO (Either Text UserInfo)
getUserInfo accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://www.googleapis.com/oauth2/v2/userinfo"
  req <- parseRequest url
  let authReq = req
        { requestHeaders = [("Authorization", "Bearer " <> encodeUtf8 accessToken)]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just ui -> Right ui
    Nothing -> Left $ "Failed to parse userinfo: " <> T.pack (show $ responseBody response)
