module Http.Handlers.Chat
  ( postChat
  ) where

import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types.Status (status400, status500)
import Web.Scotty.Trans (ActionT, json, status, jsonData)
import App.Monad (Env)
import Domain.Chat (ChatRequest(..), ChatResponse(..))
import Services.Chat (processChat)

-- POST /chat
postChat :: ActionT (ReaderT Env IO) ()
postChat = do
  req <- jsonData :: ActionT (ReaderT Env IO) ChatRequest
  let msg = chatMessage req
  if T.null (T.strip msg)
    then do
      status status400
      json $ object ["error" .= ("Message cannot be empty" :: Text)]
    else do
      result <- lift $ processChat msg
      case result of
        Left err -> do
          status status500
          json $ object ["error" .= err]
        Right response -> json $ ChatResponse response
