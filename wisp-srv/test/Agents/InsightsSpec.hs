module Agents.InsightsSpec (spec) where

import Test.Hspec
import Data.Aeson (decode, encode, object, (.=))

import Agents.Insights
import Domain.Agent (agentId, agentImplemented)

spec :: Spec
spec = do
  describe "Insights" $ do
    describe "agentInfo" $ do
      it "has correct agent ID" $ do
        agentId agentInfo `shouldBe` "wisp/insights"

      it "is marked as implemented" $ do
        agentImplemented agentInfo `shouldBe` True

    describe "SearchQuery parsing" $ do
      it "parses search_activities with query and limit" $ do
        let json = "{\"tool\": \"search_activities\", \"query\": \"meeting\", \"limit\": 10}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (SearchActivities q) -> do
            searchTerm q `shouldBe` "meeting"
            searchLimit q `shouldBe` Just 10
          _ -> expectationFailure "Expected SearchActivities"

      it "parses search_activities with only query" $ do
        let json = "{\"tool\": \"search_activities\", \"query\": \"email\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (SearchActivities q) -> do
            searchTerm q `shouldBe` "email"
            searchLimit q `shouldBe` Nothing
          _ -> expectationFailure "Expected SearchActivities"

      it "fails to parse search_activities without query" $ do
        let json = "{\"tool\": \"search_activities\"}"
        let result = decode json :: Maybe InsightsToolCall
        result `shouldBe` Nothing

    describe "SummaryQuery parsing" $ do
      it "parses get_summary with hours" $ do
        let json = "{\"tool\": \"get_summary\", \"hours\": 48}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetSummary q) -> summaryHours q `shouldBe` Just 48
          _ -> expectationFailure "Expected GetSummary"

      it "parses get_summary without hours" $ do
        let json = "{\"tool\": \"get_summary\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetSummary q) -> summaryHours q `shouldBe` Nothing
          _ -> expectationFailure "Expected GetSummary"

    describe "PeopleQuery parsing" $ do
      it "parses get_people_insights with search term" $ do
        let json = "{\"tool\": \"get_people_insights\", \"search\": \"john\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetPeopleInsights q) -> do
            peopleSearch q `shouldBe` Just "john"
            peopleImportantOnly q `shouldBe` Nothing
          _ -> expectationFailure "Expected GetPeopleInsights"

      it "parses get_people_insights with important_only" $ do
        let json = "{\"tool\": \"get_people_insights\", \"important_only\": true}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetPeopleInsights q) -> do
            peopleSearch q `shouldBe` Nothing
            peopleImportantOnly q `shouldBe` Just True
          _ -> expectationFailure "Expected GetPeopleInsights"

      it "parses get_people_insights with all parameters" $ do
        let json = "{\"tool\": \"get_people_insights\", \"search\": \"acme\", \"important_only\": false}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetPeopleInsights q) -> do
            peopleSearch q `shouldBe` Just "acme"
            peopleImportantOnly q `shouldBe` Just False
          _ -> expectationFailure "Expected GetPeopleInsights"

      it "parses get_people_insights with no parameters" $ do
        let json = "{\"tool\": \"get_people_insights\"}"
        let result = decode json :: Maybe InsightsToolCall
        case result of
          Just (GetPeopleInsights q) -> do
            peopleSearch q `shouldBe` Nothing
            peopleImportantOnly q `shouldBe` Nothing
          _ -> expectationFailure "Expected GetPeopleInsights"

    describe "Unknown tool handling" $ do
      it "fails to parse unknown tool" $ do
        let json = "{\"tool\": \"unknown_tool\"}"
        let result = decode json :: Maybe InsightsToolCall
        result `shouldBe` Nothing

    describe "ToolResult encoding" $ do
      it "encodes ToolSuccess correctly" $ do
        let result = ToolSuccess (object ["count" .= (5 :: Int)])
        -- ToolSuccess wraps with success field
        let encoded = encode result
        encoded `shouldNotBe` ""

      it "encodes ToolError correctly" $ do
        let result = ToolError "Something went wrong"
        let encoded = encode result
        -- Should contain the error message
        encoded `shouldNotBe` ""
