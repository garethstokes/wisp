-- src/Infra/Google/TokenManager.hs
module Infra.Google.TokenManager
  ( getValidToken
  , TokenError(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import App.Monad (App, getConfig)
import App.Config (Config(..), GoogleConfig(..))
import Infra.Db.Auth (AuthToken(..), getToken, updateToken, tokenNeedsRefresh)
import Infra.Google.Auth (OAuthConfig(..), refreshAccessToken, TokenResponse(..))

data TokenError
  = NoToken
  | RefreshFailed Text
  deriving (Show, Eq)

-- Build OAuthConfig from app config
mkOAuthConfig :: Config -> OAuthConfig
mkOAuthConfig cfg = OAuthConfig
  { oauthClientId = cfg.google.clientId
  , oauthClientSecret = cfg.google.clientSecret
  , oauthRedirectUri = "http://127.0.0.1:8080/auth/google/callback"
  }

-- Get a valid access token, refreshing if needed
getValidToken :: App (Either TokenError Text)
getValidToken = do
  mtoken <- getToken "google"
  case mtoken of
    Nothing -> pure $ Left NoToken
    Just tok -> do
      needsRefresh <- liftIO $ tokenNeedsRefresh tok
      if needsRefresh
        then refreshAndUpdate tok
        else pure $ Right (tokenAccessToken tok)

-- Refresh the token and update in database
refreshAndUpdate :: AuthToken -> App (Either TokenError Text)
refreshAndUpdate tok = do
  cfg <- getConfig
  let oauthCfg = mkOAuthConfig cfg
  result <- liftIO $ refreshAccessToken oauthCfg (tokenRefreshToken tok)
  case result of
    Left err -> pure $ Left (RefreshFailed err)
    Right newTok -> do
      now <- liftIO getCurrentTime
      let newExpiry = addUTCTime (fromIntegral $ expiresIn newTok) now
      updateToken "google" (accessToken newTok) newExpiry
      pure $ Right (accessToken newTok)
