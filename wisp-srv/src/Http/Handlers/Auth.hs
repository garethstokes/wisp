module Http.Handlers.Auth
  ( getGoogleAuth
  , getGoogleCallback
  , getGitHubAuth
  , getGitHubCallback
  , getAuthStatus
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (Value, object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Network.HTTP.Types.Status (status302, status400, status500)
import Web.Scotty.Trans (ActionT, json, status, queryParamMaybe, setHeader)
import App.Monad (Env, getConfig)
import App.Config (Config(..), ServerConfig(..), GoogleConfig(..), GitHubConfig(..))
import Infra.Google.Auth (OAuthConfig(..), buildAuthUrl, exchangeCode, getUserInfo, TokenResponse(..), UserInfo(..))
import Infra.GitHub.Auth (GitHubOAuthConfig(..), buildGitHubAuthUrl, exchangeGitHubCode, getGitHubUser, GitHubUser(..), GitHubTokenResponse(..))
import Infra.Db.Auth (saveToken)
import Infra.Db.Account (upsertAccount, upsertAccountByProvider, getAllAccounts)
import Infra.Db.Tenant (getAllTenants)
import Domain.Account (Account(..), AccountProvider(..), accountIdentifier)
import Domain.Tenant (Tenant(..))

-- Build OAuthConfig from app config (Google)
mkOAuthConfig :: Config -> OAuthConfig
mkOAuthConfig cfg = OAuthConfig
  { oauthClientId = cfg.google.clientId
  , oauthClientSecret = cfg.google.clientSecret
  , oauthRedirectUri = "http://127.0.0.1:" <> T.pack (show cfg.server.port) <> "/auth/google/callback"
  }

-- Build GitHubOAuthConfig from app config
mkGitHubOAuthConfig :: Config -> Maybe GitHubOAuthConfig
mkGitHubOAuthConfig cfg = case cfg.github of
  Nothing -> Nothing
  Just gh -> Just $ GitHubOAuthConfig
    { ghClientId = gh.clientId
    , ghClientSecret = gh.clientSecret
    , ghRedirectUri = "http://127.0.0.1:" <> T.pack (show cfg.server.port) <> "/auth/github/callback"
    }

-- Redirect to Google OAuth
getGoogleAuth :: ActionT (ReaderT Env IO) ()
getGoogleAuth = do
  cfg <- lift getConfig
  let oauthCfg = mkOAuthConfig cfg
  let url = buildAuthUrl oauthCfg
  setHeader "Location" (TL.fromStrict url)
  status status302
  json $ object ["redirect" .= url]

-- Handle OAuth callback
getGoogleCallback :: ActionT (ReaderT Env IO) ()
getGoogleCallback = do
  mcode <- queryParamMaybe "code"
  merror <- queryParamMaybe "error"

  case merror of
    Just err -> do
      status status400
      json $ object ["error" .= (err :: Text)]
    Nothing -> case mcode of
      Nothing -> do
        status status400
        json $ object ["error" .= ("Missing authorization code" :: Text)]
      Just code -> do
        cfg <- lift getConfig
        let oauthCfg = mkOAuthConfig cfg
        result <- liftIO $ exchangeCode oauthCfg code
        case result of
          Left err -> do
            status status500
            json $ object ["error" .= err]
          Right tok -> do
            -- Get user info to auto-detect account
            userInfoResult <- liftIO $ getUserInfo (accessToken tok)
            case userInfoResult of
              Left err -> do
                status status500
                json $ object ["error" .= ("Failed to get user info: " <> err)]
              Right userInfo -> do
                -- Upsert the account based on email
                account <- lift $ upsertAccount (userEmail userInfo) (userName userInfo)

                -- Save token for this account
                now <- liftIO getCurrentTime
                let expiresAt = addUTCTime (fromIntegral $ expiresIn tok) now
                let refreshTok = maybe "" id (refreshToken tok)
                _ <- lift $ saveToken (accountId account) "google" (accessToken tok) refreshTok expiresAt
                  ["https://www.googleapis.com/auth/gmail.readonly"
                  ,"https://www.googleapis.com/auth/calendar.readonly"
                  ]
                json $ object
                  [ "status" .= ("authenticated" :: Text)
                  , "email" .= accountIdentifier account
                  , "expires_at" .= expiresAt
                  ]

-- GitHub OAuth redirect
getGitHubAuth :: ActionT (ReaderT Env IO) ()
getGitHubAuth = do
  cfg <- lift getConfig
  case mkGitHubOAuthConfig cfg of
    Nothing -> do
      status status500
      json $ object ["error" .= ("GitHub OAuth not configured" :: Text)]
    Just oauthCfg -> do
      let url = buildGitHubAuthUrl oauthCfg
      setHeader "Location" (TL.fromStrict url)
      status status302
      json $ object ["redirect" .= url]

-- GitHub OAuth callback
getGitHubCallback :: ActionT (ReaderT Env IO) ()
getGitHubCallback = do
  mcode <- queryParamMaybe "code"
  merror <- queryParamMaybe "error"

  case merror of
    Just err -> do
      status status400
      json $ object ["error" .= (err :: Text)]
    Nothing -> case mcode of
      Nothing -> do
        status status400
        json $ object ["error" .= ("Missing authorization code" :: Text)]
      Just code -> do
        cfg <- lift getConfig
        case mkGitHubOAuthConfig cfg of
          Nothing -> do
            status status500
            json $ object ["error" .= ("GitHub OAuth not configured" :: Text)]
          Just oauthCfg -> do
            result <- liftIO $ exchangeGitHubCode oauthCfg code
            case result of
              Left err -> do
                status status500
                json $ object ["error" .= err]
              Right tok -> do
                -- Get user info
                userResult <- liftIO $ getGitHubUser (ghAccessToken tok)
                case userResult of
                  Left err -> do
                    status status500
                    json $ object ["error" .= ("Failed to get user info: " <> err)]
                  Right user -> do
                    -- Create account with GitHub details
                    let details = Aeson.object
                          [ "username" Aeson..= ghUserLogin user
                          , "access_token" Aeson..= ghAccessToken tok
                          ]
                    _account <- lift $ upsertAccountByProvider GitHub details (ghUserName user)
                    json $ object
                      [ "status" .= ("authenticated" :: Text)
                      , "username" .= ghUserLogin user
                      , "provider" .= ("github" :: Text)
                      ]

-- Check auth status (shows all connected accounts and active tenant)
getAuthStatus :: ActionT (ReaderT Env IO) ()
getAuthStatus = do
  accounts <- lift getAllAccounts
  tenants <- lift getAllTenants
  now <- liftIO getCurrentTime
  let accountInfos = map (accountToJson now) accounts
  -- For now, use the first tenant as "active" (TODO: get from session/token)
  let activeTenant = case tenants of
        (t:_) -> Just $ tenantToJson t
        [] -> Nothing
  json $ object
    [ "accounts" .= accountInfos
    , "count" .= length accounts
    , "tenant" .= activeTenant
    ]

-- Convert account to JSON object
accountToJson :: UTCTime -> Account -> Data.Aeson.Value
accountToJson _ acc = object
  [ "provider" .= accountProvider acc
  , "identifier" .= accountIdentifier acc
  , "display_name" .= accountDisplayName acc
  , "created_at" .= accountCreatedAt acc
  ]

-- Convert tenant to JSON object
tenantToJson :: Tenant -> Data.Aeson.Value
tenantToJson t = object
  [ "id" .= tenantId t
  , "name" .= tenantName t
  ]
