-- src/Infra/Google/TokenManager.hs
module Infra.Google.TokenManager
  ( getValidToken
  , getValidTokenForAccount
  , getAllValidTokens
  , TokenError(..)
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (addUTCTime, getCurrentTime)
import App.Monad (App, getConfig)
import App.Config (Config(..), GoogleConfig(..))
import Domain.Id (EntityId)
import Domain.Account (Account(..), AccountProvider(..))
import Infra.Db.Auth (AuthToken(..), getTokenForAccount, getAllTokens, updateTokenForAccount, tokenNeedsRefresh)
import Infra.Db.Account (getAccountsByProvider)
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

-- Get a valid access token for a specific account
getValidTokenForAccount :: EntityId -> App (Either TokenError Text)
getValidTokenForAccount accountId = do
  mtoken <- getTokenForAccount accountId "google"
  case mtoken of
    Nothing -> pure $ Left NoToken
    Just tok -> do
      needsRefresh <- liftIO $ tokenNeedsRefresh tok
      if needsRefresh
        then refreshAndUpdateForAccount accountId tok
        else pure $ Right (tokenAccessToken tok)

-- Get valid tokens for all Google accounts (for polling)
getAllValidTokens :: App [(Account, Either TokenError Text)]
getAllValidTokens = do
  accounts <- getAccountsByProvider Google
  forM accounts $ \acc -> do
    tokenResult <- getValidTokenForAccount (accountId acc)
    pure (acc, tokenResult)

-- Legacy: get any valid token (first account found)
getValidToken :: App (Either TokenError Text)
getValidToken = do
  tokens <- getAllTokens "google"
  case tokens of
    [] -> pure $ Left NoToken
    (tok:_) -> do
      needsRefresh <- liftIO $ tokenNeedsRefresh tok
      if needsRefresh
        then refreshAndUpdateForAccount (tokenAccountId tok) tok
        else pure $ Right (tokenAccessToken tok)

-- Refresh the token and update in database for a specific account
refreshAndUpdateForAccount :: EntityId -> AuthToken -> App (Either TokenError Text)
refreshAndUpdateForAccount accountId tok = do
  cfg <- getConfig
  let oauthCfg = mkOAuthConfig cfg
  result <- liftIO $ refreshAccessToken oauthCfg (tokenRefreshToken tok)
  case result of
    Left err -> pure $ Left (RefreshFailed err)
    Right newTok -> do
      now <- liftIO getCurrentTime
      let newExpiry = addUTCTime (fromIntegral $ expiresIn newTok) now
      updateTokenForAccount accountId "google" (accessToken newTok) newExpiry
      pure $ Right (accessToken newTok)
