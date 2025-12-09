{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: Kipu.GitHub.GraphQL.Client
-- Description: Client for GitHub GraphQL API.
-- Copyright: (c) Marco Benelli 2025
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
module Kipu.GitHub.GraphQL.Client where

import           BasicPrelude
import           Control.Monad.Reader
import           Data.Aeson
import           GHC.Generics              (Generic)
import           Kipu.Config
import           Kipu.GitHub.GraphQL.Types
import           Network.HTTP.Req

newtype Query r = Query
  { query :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type Token = ByteString

schema :: Query Object
schema = Query {query = "query { __schema { types { name kind description fields { name }}}}"}

typeList :: Query Object
typeList = Query {query = "query {__schema { types { name }}}"}


-- Repositories in an organization
--
orgRepos :: Text -> Int -> Maybe Text -> Query RepoResult
orgRepos org f cursor = Query {
  query = "query { \
\    organization(login:\"" <> org <> "\") { \
\      repositories(first: " <> tshow f <> " " <> after <>  ") { \
\       nodes { \
\           archivedAt \
\           createdAt \
\           isArchived \
\           isDisabled \
\           isEmpty \
\           isFork \
\           isLocked \
\           isMirror \
\           isPrivate \
\           isTemplate \
\           name \
\           stargazerCount \
\           updatedAt \
\           url \
\       } \
\       pageInfo { \
\         endCursor \
\         startCursor \
\         hasNextPage \
\         hasPreviousPage \
\       }}}}}"
  }
  where
    after = case cursor of
      Nothing -> "after: null"
      Just c  -> "after: \"" <> c <> "\""

-- | Push Requests in a repository
--
prQuery:: Text -> Text -> Int -> Maybe Text -> Query PrResult
prQuery owner repo f cursor = Query {
  query = "query { \
\   repository(name: \"" <> repo <> "\" owner: \"" <> owner <> "\") { \
\     pullRequests (first: " <> tshow f <> " " <> after <> ") { \
\       nodes { \
\         author { \
\           login \
\         } \
\         baseRefName \
\         baseRefOid \
\         closed \
\         closedAt \
\         commits (first: 100 after: null) { \
\           nodes { \
\             commit { \
\               authors (first: 1 after: null) { \
\                 nodes { \
\                   date \
\                   email \
\                   name \
\                   user { \
\                     login \
\                   }\
\                 } \
\                 pageInfo { \
\                   endCursor \
\                   startCursor \
\                   hasNextPage \
\                   hasPreviousPage \
\                 } \
\                 totalCount \
\               } \
\               abbreviatedOid \
\             } \
\           } \
\           pageInfo { \
\             endCursor \
\             startCursor \
\             hasNextPage \
\             hasPreviousPage \
\           }\
\           totalCount \
\         } \
\         createdAt \
\         id \
\         merged \
\         mergedAt \
\         number \
\         permalink \
\         publishedAt \
\         state \
\         title \
\         updatedAt \
\         url \
\       }\
\       pageInfo { \
\         endCursor \
\         startCursor \
\         hasNextPage \
\         hasPreviousPage \
\       }\
\       totalCount \
\     } \
\   }}"
  }
  where
    after = case cursor of
      Nothing -> "after: null"
      Just c  -> "after: \"" <> c <> "\""

type ClientM a = ReaderT Config IO a

call :: Paged r => Query r -> ClientM r
call q = do
  c <- ask
  let t = encodeUtf8 $ githubToken c
      ep = https "api.github.com" /: "graphql"
      body = ReqBodyJson q
      prms =
        mconcat
          [ header "Authorization" ("bearer " <> t)
          , header "User-Agent" "agent"
          ]
      request = req POST ep body jsonResponse prms
  responseBody <$> runReq defaultHttpConfig request

paged:: Paged r => (Int -> Maybe Text -> Query r) -> ClientM [ResultValue r]
paged q = go 100 Nothing
  where
    go from after = do
          res <- call $ q from after
          let rs = values res
          let page = pageInfo res
          if hasNextPage page then
            (rs <>) <$> go from (Just (endCursor page))
          else
            pure rs

run :: Paged r => (Int -> Maybe Text -> Query r) -> IO [ResultValue r]
run f = do
  file <- defaultConfigFile
  cfg <- readConfig file
  case cfg of
    Left e  -> fail $ show e
    Right c -> runReaderT (paged f) c

repos :: Text -> IO [Repository]
repos org = run $ orgRepos org

prs :: Text -> Text -> IO [PullRequest]
prs org repo = run $ prQuery org repo
