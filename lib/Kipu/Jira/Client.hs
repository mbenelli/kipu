{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module: Kipu.Jira.Client
Description: Client for Jira API
Copyright: (c) Marco Benelli 2025
License: ISC
Maintainer: mbenelli@fastmail.com
-}
module Kipu.Jira.Client where

import           BasicPrelude
import           Control.Monad.Reader
import           Data.Aeson            (FromJSON, ToJSON)
import           Kipu.Config
import           Kipu.Jira.ApiTypes
import           Kipu.Jira.CustomTypes (IssueBean)
import qualified Kipu.Jira.Types       as JT
import           Network.HTTP.Req

type ClientM = ReaderT Config IO

post :: (ToJSON a, FromJSON b) => [Text] -> a -> ClientM b
post p b = do
  c <- ask
  let t = encodeUtf8 $ token c
      u = url c
      usr = encodeUtf8 $ user c
      ep = foldl' (/:) (https u) p
      body = ReqBodyJson b
      prms =
        mconcat
          [ basicAuth usr t
          , header "User-Agent" "agent"
          ]
      request = req POST ep body jsonResponse prms
  responseBody <$> runReq defaultHttpConfig request

get:: (FromJSON a) => [Text] -> ClientM a
get p = do
  c <- ask
  let t = encodeUtf8 $ token c
      u = url c
      usr = encodeUtf8 $ user c
      ep = foldl' (/:) (https u) p
      prms =
        mconcat
          [ basicAuth usr t
          , header "User-Agent" "agent"
          ]
      request = req GET ep NoReqBody jsonResponse prms
  responseBody <$> runReq defaultHttpConfig request

jql :: SearchRequest -> ClientM SearchResponse
jql = post ["rest", "api", "3", "search", "jql"]

fieldDetails :: ClientM [JT.FieldDetails]
fieldDetails = get ["rest", "api", "3", "field"]

issueType:: ClientM [JT.IssueTypeDetails]
issueType = get ["rest", "api", "3", "issuetype"]

createIssue :: CreateIssueRequest -> ClientM IssueBean
createIssue = post ["rest", "api", "2", "issue"]

issue :: Int -> ClientM IssueBean
issue i = get ["rest", "api", "2", "issue", tshow i]

changelog :: Int -> ClientM JT.PageBeanChangelog
changelog i = get ["rest", "api", "2", "issue", tshow i]

-- TODO
--
-- workspace :: ClientM Text
-- workspace :: get ["rest", "servicedeskapi", "assets", "workspace"]

iql :: Text -> Int -> ClientM InsightSearchResponse
iql q p = get ["rest", "insight", "1.0", "iql", "objects",
    q, tshow p
  ]

runClient :: ClientM a -> IO a
runClient client = do
  file <- defaultConfigFile
  cfg <- readConfig file
  case cfg of
    Left e  -> fail $ show e
    Right c -> runReaderT client c

runJql :: Text -> IO [IssueBean]
runJql query =  go query Nothing []
  where
    go q t a = do
      res <- runClient $ jql $ searchRequest q t
      let n = a <> searchResponse_issues res
      if searchResponse_isLast res
      then pure n
      else go q (searchResponse_nextPageToken res) n
