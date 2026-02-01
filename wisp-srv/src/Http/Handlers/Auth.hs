module Http.Handlers.Auth
  ( getGoogleAuth
  , getGoogleCallback
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (addUTCTime, getCurrentTime)
import Network.HTTP.Types.Status (status302, status400, status500)
import Web.Scotty.Trans (ActionT, json, status, queryParamMaybe, setHeader)
import App.Monad (Env, getConfig)
import App.Config (Config(..), ServerConfig(..), GoogleConfig(..))
import Infra.Google.Auth (OAuthConfig(..), buildAuthUrl, exchangeCode, TokenResponse(..))
import Infra.Db.Auth (saveToken)

-- Build OAuthConfig from app config
mkOAuthConfig :: Config -> OAuthConfig
mkOAuthConfig cfg = OAuthConfig
  { oauthClientId = cfg.google.clientId
  , oauthClientSecret = cfg.google.clientSecret
  , oauthRedirectUri = "http://127.0.0.1:" <> T.pack (show cfg.server.port) <> "/auth/google/callback"
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
            now <- liftIO getCurrentTime
            let expiresAt = addUTCTime (fromIntegral $ expiresIn tok) now
            let refreshTok = maybe "" id (refreshToken tok)
            _ <- lift $ saveToken "google" (accessToken tok) refreshTok expiresAt
              ["https://www.googleapis.com/auth/gmail.readonly"
              ,"https://www.googleapis.com/auth/calendar.readonly"
              ]
            json $ object
              [ "status" .= ("authenticated" :: Text)
              , "expires_at" .= expiresAt
              ]
