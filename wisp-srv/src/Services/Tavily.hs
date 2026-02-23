-- | Tavily Web Search Service
-- Provides web search capabilities via the Tavily API.
module Services.Tavily
  ( TavilySearchResult(..)
  , search
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON(..), ToJSON(..), Value(..), object, withObject, (.:), (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS

import App.Monad (App, getConfig)
import App.Config (Config(..))

-- | A single search result from Tavily
data TavilySearchResult = TavilySearchResult
  { resultTitle :: Text
  , resultUrl :: Text
  , resultContent :: Text
  , resultScore :: Double
  } deriving (Show, Eq, Generic)

instance ToJSON TavilySearchResult where
  toJSON r = object
    [ "title" .= resultTitle r
    , "url" .= resultUrl r
    , "content" .= resultContent r
    , "score" .= resultScore r
    ]

instance FromJSON TavilySearchResult where
  parseJSON = withObject "TavilySearchResult" $ \v -> TavilySearchResult
    <$> v .: "title"
    <*> v .: "url"
    <*> v .: "content"
    <*> v .:? "score" .!= 0.0

-- | Response from Tavily search API
data TavilyResponse = TavilyResponse
  { tavilyResults :: [TavilySearchResult]
  } deriving (Show)

instance FromJSON TavilyResponse where
  parseJSON = withObject "TavilyResponse" $ \v -> TavilyResponse
    <$> v .: "results"

(.!= ) :: Aeson.Parser (Maybe a) -> a -> Aeson.Parser a
(.!= ) p def = p >>= maybe (pure def) pure

-- | Search the web using Tavily API
-- Returns an error if API key is not configured or request fails
search :: Text -> Int -> App (Either Text [TavilySearchResult])
search query maxResults = do
  cfg <- getConfig
  case tavily cfg of
    Nothing -> pure $ Left "Tavily API key not configured"
    Just tavilyConfig -> do
      let apiKey = App.Config.apiKey tavilyConfig
      liftIO $ searchWithKey apiKey query maxResults

-- | Perform search with API key
searchWithKey :: Text -> Text -> Int -> IO (Either Text [TavilySearchResult])
searchWithKey apiKey query maxResults = do
  manager <- newManager tlsManagerSettings

  let requestBody = object
        [ "api_key" .= apiKey
        , "query" .= query
        , "max_results" .= maxResults
        , "include_answer" .= False
        , "include_raw_content" .= False
        ]

  initReq <- parseRequest "https://api.tavily.com/search"
  let req = initReq
        { method = "POST"
        , requestHeaders =
            [ ("Content-Type", "application/json")
            , ("User-Agent", "wisp-srv")
            ]
        , requestBody = RequestBodyLBS $ Aeson.encode requestBody
        }

  response <- httpLbs req manager
  let status = statusCode (responseStatus response)

  case status of
    200 -> case Aeson.decode (responseBody response) of
      Just resp -> pure $ Right $ tavilyResults resp
      Nothing -> pure $ Left "Failed to parse Tavily response"
    401 -> pure $ Left "Invalid Tavily API key"
    429 -> pure $ Left "Tavily rate limit exceeded"
    _ -> pure $ Left $ "Tavily API error: " <> T.pack (show status)
