module Infra.GitHub.Auth
  ( GitHubOAuthConfig(..)
  , GitHubTokenResponse(..)
  , GitHubUser(..)
  , buildGitHubAuthUrl
  , exchangeGitHubCode
  , getGitHubUser
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

data GitHubOAuthConfig = GitHubOAuthConfig
  { ghClientId :: Text
  , ghClientSecret :: Text
  , ghRedirectUri :: Text
  } deriving (Show)

data GitHubTokenResponse = GitHubTokenResponse
  { ghAccessToken :: Text
  , ghTokenType :: Text
  , ghScope :: Text
  } deriving (Show)

instance FromJSON GitHubTokenResponse where
  parseJSON = withObject "GitHubTokenResponse" $ \v -> GitHubTokenResponse
    <$> v .: "access_token"
    <*> v .: "token_type"
    <*> v .: "scope"

data GitHubUser = GitHubUser
  { ghUserLogin :: Text
  , ghUserName :: Maybe Text
  , ghUserAvatarUrl :: Maybe Text
  } deriving (Show, Eq, Generic)

instance FromJSON GitHubUser where
  parseJSON = withObject "GitHubUser" $ \v -> GitHubUser
    <$> v .: "login"
    <*> v .:? "name"
    <*> v .:? "avatar_url"

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

-- Build the GitHub OAuth authorization URL
buildGitHubAuthUrl :: GitHubOAuthConfig -> Text
buildGitHubAuthUrl cfg = T.concat
  [ "https://github.com/login/oauth/authorize"
  , "?client_id=", urlEncode (ghClientId cfg)
  , "&redirect_uri=", urlEncode (ghRedirectUri cfg)
  , "&scope=", urlEncode "read:user"
  ]

-- Exchange authorization code for access token
exchangeGitHubCode :: GitHubOAuthConfig -> Text -> IO (Either Text GitHubTokenResponse)
exchangeGitHubCode cfg code = do
  manager <- newManager tlsManagerSettings
  let body = BS.intercalate "&"
        [ "client_id=" <> encodeUtf8 (ghClientId cfg)
        , "client_secret=" <> encodeUtf8 (ghClientSecret cfg)
        , "code=" <> encodeUtf8 code
        , "redirect_uri=" <> encodeUtf8 (ghRedirectUri cfg)
        ]
  initialReq <- parseRequest "https://github.com/login/oauth/access_token"
  let req = initialReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/x-www-form-urlencoded")
            , ("Accept", "application/json")
            ]
        , requestBody = RequestBodyBS body
        }
  response <- httpLbs req manager
  pure $ case Aeson.decode (responseBody response) of
    Just tok -> Right tok
    Nothing -> Left $ "Failed to parse token response: " <> T.pack (show $ responseBody response)

-- Fetch GitHub user info using access token
getGitHubUser :: Text -> IO (Either Text GitHubUser)
getGitHubUser accessToken = do
  manager <- newManager tlsManagerSettings
  let url = "https://api.github.com/user"
  req <- parseRequest url
  let authReq = req
        { requestHeaders =
            [ ("Authorization", "Bearer " <> encodeUtf8 accessToken)
            , ("User-Agent", "wisp-srv")
            ]
        }
  response <- httpLbs authReq manager
  pure $ case Aeson.decode (responseBody response) of
    Just ui -> Right ui
    Nothing -> Left $ "Failed to parse user info: " <> T.pack (show $ responseBody response)
