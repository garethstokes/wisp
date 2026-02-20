-- wisp-core/src/Wisp/Client/Types.hs
module Wisp.Client.Types
  ( ClientConfig(..)
  , ClientError(..)
  , defaultConfig
  ) where

import Data.Text (Text)

data ClientConfig = ClientConfig
  { configBaseUrl :: Text
  , configTimeout :: Int  -- seconds
  } deriving (Show, Eq)

defaultConfig :: ClientConfig
defaultConfig = ClientConfig
  { configBaseUrl = "http://127.0.0.1:5812"
  , configTimeout = 30
  }

data ClientError
  = HttpError Text
  | ParseError Text
  | ServerError Int Text
  deriving (Show, Eq)
