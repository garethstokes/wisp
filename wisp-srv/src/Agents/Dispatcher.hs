-- | Agent Dispatcher
-- Routes chat to knowledge-based agents loaded from the database.
-- Agents are defined by activities with "agent:NAME" tags.
module Agents.Dispatcher
  ( dispatchChat
  , listAgentNamesByTenant
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Domain.Agent (AgentName, parseAgentTag)
import Domain.Chat (ChatMessage(..), ChatResponse(..))
import Domain.Tenant (TenantId)
import Domain.Id (EntityId)
import App.Monad (App)
import Agents.Core (Agent(..), loadAgentByTenant, buildSystemPrompt, loadSkillPromptByTenant, executeTool, ToolExecutionResult(..))
import Agents.Run (RunContext, withRunLogging, callClaudeLogged, logToolRequest, logToolSuccess, logToolFailure)
import Infra.Db.Activity (searchTagsByTenant)
import Skills.Registry (Skill(..))
import Data.Aeson (Value, decode, encode)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL

-- | Dispatch chat to a knowledge-based agent
-- Loads the agent from knowledge, builds system prompt, calls LLM, handles tool calls
-- Implements an agentic loop: call LLM -> execute tools -> feed results back -> repeat
-- mTimezone: Optional IANA timezone for date/time formatting in tools
dispatchChat :: TenantId -> EntityId -> Text -> [ChatMessage] -> Maybe Text -> App (Either Text ChatResponse)
dispatchChat tenantId accountId agentName msgs mTimezone = do
  -- Load the agent from knowledge
  mAgent <- loadAgentByTenant tenantId agentName
  case mAgent of
    Nothing -> pure $ Left $ "Unknown agent: " <> agentName
    Just agent -> withRunLogging agentName Nothing msgs $ \ctx messages -> do
      -- Load skill prompt if skill is active
      mSkillPrompt <- case agentSkill agent of
        Nothing -> pure Nothing
        Just skill -> loadSkillPromptByTenant tenantId (Skills.Registry.skillName skill)

      -- Build the system prompt
      let systemPrompt = buildSystemPrompt agent mSkillPrompt

      -- Run the agentic loop
      runAgentLoop ctx agent accountId systemPrompt messages [] mTimezone

-- | Agentic loop: call LLM, execute tools, feed results back, repeat until done
runAgentLoop
  :: RunContext
  -> Agent
  -> EntityId
  -> Text                    -- System prompt
  -> [ChatMessage]           -- Original conversation
  -> [(Text, Value, Value)]  -- Accumulated tool results: (toolName, args, result)
  -> Maybe Text              -- Timezone
  -> App (Either Text ChatResponse)
runAgentLoop ctx agent accountId systemPrompt messages prevToolResults mTimezone = do
  -- Build user prompt including any tool results
  let userPrompt = formatMessagesWithTools messages prevToolResults

  -- Call Claude
  result <- callClaudeLogged ctx systemPrompt userPrompt
  case result of
    Left err -> pure $ Left err
    Right response -> do
      -- Parse the response (expects {msg, tools} format)
      case parseLLMResponse response of
        Left _err ->
          -- Fallback: treat as plain text response
          pure $ Right ChatResponse
            { responseMessage = response
            , responseToolCall = Nothing
            }
        Right (msg, toolCalls) ->
          case toolCalls of
            [] ->
              -- No tools - we're done
              pure $ Right ChatResponse
                { responseMessage = msg
                , responseToolCall = Nothing
                }
            tools -> do
              -- Execute all tools
              toolResults <- mapM (executeToolLogged ctx agent accountId mTimezone) tools
              -- Check for permission requests or errors
              case findBlocker toolResults of
                Just blocker -> pure $ Right blocker
                Nothing -> do
                  -- Continue loop with tool results
                  let newResults = [(name, args, res) | (name, args, ToolExecSuccess res) <- toolResults]
                  runAgentLoop ctx agent accountId systemPrompt messages newResults mTimezone

-- | Execute a tool and log it
executeToolLogged
  :: RunContext
  -> Agent
  -> EntityId
  -> Maybe Text
  -> (Text, Value)  -- (toolName, args)
  -> App (Text, Value, ToolExecutionResult)
executeToolLogged ctx agent accountId mTimezone (toolName, toolArgs) = do
  _ <- logToolRequest ctx toolName toolArgs
  toolResult <- executeTool agent accountId toolName toolArgs mTimezone
  case toolResult of
    ToolExecSuccess val -> logToolSuccess ctx toolName val
    ToolExecError err -> logToolFailure ctx toolName err
    ToolExecPermission _ _ -> pure ()  -- Don't log permissions as success/failure
  pure (toolName, toolArgs, toolResult)

-- | Find any blocker (error or permission request) in tool results
findBlocker :: [(Text, Value, ToolExecutionResult)] -> Maybe ChatResponse
findBlocker [] = Nothing
findBlocker ((toolName, _, ToolExecError err):_) =
  Just ChatResponse
    { responseMessage = "Tool error (" <> toolName <> "): " <> err
    , responseToolCall = Nothing
    }
findBlocker ((_, _, ToolExecPermission action msg):_) =
  Just ChatResponse
    { responseMessage = msg
    , responseToolCall = Just $ Aeson.object
        [ "action" Aeson..= action
        , "requires_permission" Aeson..= True
        ]
    }
findBlocker (_:rest) = findBlocker rest

-- | Format chat messages into a user prompt
formatMessages :: [ChatMessage] -> Text
formatMessages msgs = T.intercalate "\n\n" $ map formatMsg msgs
  where
    formatMsg msg = case messageRole msg of
      "user" -> "User: " <> messageContent msg
      "assistant" -> "Assistant: " <> messageContent msg
      role -> role <> ": " <> messageContent msg

-- | Format messages with tool results from previous turn
formatMessagesWithTools :: [ChatMessage] -> [(Text, Value, Value)] -> Text
formatMessagesWithTools msgs [] = formatMessages msgs
formatMessagesWithTools msgs toolResults =
  formatMessages msgs <> "\n\n## Tool Results\nYou called these tools. Here are the results:\n\n" <>
  T.intercalate "\n\n" (map formatToolResult toolResults)
  where
    formatToolResult (toolName, _args, result) =
      "### " <> toolName <> "\n" <> decodeUtf8 (BL.toStrict (encode result))

-- | Parse LLM response in {msg, tools} format
-- Returns (message, [(toolName, toolArgs)])
parseLLMResponse :: Text -> Either Text (Text, [(Text, Value)])
parseLLMResponse response = do
  let trimmed = T.strip response
  -- Extract JSON if wrapped in other text
  let json = extractJson trimmed
  case decode (BL.fromStrict $ encodeUtf8 json) :: Maybe (KM.KeyMap Value) of
    Nothing -> Left "Failed to parse JSON response"
    Just obj -> do
      msg <- case KM.lookup "msg" obj of
        Just (Aeson.String s) -> Right s
        _ -> Left "Missing or invalid 'msg' field"
      let tools = case KM.lookup "tools" obj of
            Just (Aeson.Array arr) -> [parseToolObj v | v <- foldr (:) [] arr]
            _ -> []
      Right (msg, [t | Just t <- tools])
  where
    parseToolObj :: Value -> Maybe (Text, Value)
    parseToolObj (Aeson.Object o) = do
      toolName <- KM.lookup "tool" o >>= getString
      -- The whole object (minus "tool") is the args
      let args = Aeson.Object (KM.delete "tool" o)
      Just (toolName, args)
    parseToolObj _ = Nothing

    getString (Aeson.String s) = Just s
    getString _ = Nothing

-- | Extract JSON object from response (handles markdown code blocks, etc.)
extractJson :: Text -> Text
extractJson t =
  let stripped = stripCodeBlock t
      startIdx = T.findIndex (== '{') stripped
      endIdx = findLastIndex (== '}') stripped
  in case (startIdx, endIdx) of
    (Just s, Just e) | e >= s -> T.drop s $ T.take (e + 1) stripped
    _ -> stripped

findLastIndex :: (Char -> Bool) -> Text -> Maybe Int
findLastIndex p t =
  let len = T.length t
      indices = [i | i <- [0..len-1], p (T.index t i)]
  in if null indices then Nothing else Just (last indices)

stripCodeBlock :: Text -> Text
stripCodeBlock t =
  let lines' = T.lines t
      withoutStart = case lines' of
        (l:rest) | "```" `T.isPrefixOf` l -> rest
        other -> other
      withoutEnd = case reverse withoutStart of
        (l:rest) | l == "```" -> reverse rest
        other -> reverse other
  in T.unlines withoutEnd

-- | List available agent names from knowledge (tenant-scoped)
-- Finds all tags matching "agent:*" pattern and extracts agent names
listAgentNamesByTenant :: TenantId -> App [AgentName]
listAgentNamesByTenant tenantId = do
  -- Search for tags starting with "agent:"
  tags <- searchTagsByTenant tenantId "agent:" 100
  pure $ extractAgentNames tags
  where
    extractAgentNames :: [Text] -> [AgentName]
    extractAgentNames = foldr addIfAgent []

    addIfAgent :: Text -> [AgentName] -> [AgentName]
    addIfAgent tag acc = case parseAgentTag tag of
      Just name -> name : acc
      Nothing -> acc
