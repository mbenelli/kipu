{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Kipu.GitHub.Client
-- Description: Client for GitHub
-- Copyright: (c) Marco Benelli 2025
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.GitHub.GraphQL.Client where

import           BasicPrelude
import           Control.Monad.Reader
import           Data.Aeson
import           GHC.Generics              (Generic)
import           Kipu.Config
import           Kipu.GitHub.GraphQL.Types
import           Network.HTTP.Req

newtype Query = Query
  { query :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type Token = ByteString

schema :: Query
schema = Query {query = "query { __schema { types { name kind description fields { name }}}}"}

typeList :: Query
typeList = Query {query = "query {__schema { types { name }}}"}


-- Repositories in an organization
--
orgRepos :: Text -> Int -> Maybe Text -> Query
orgRepos org f cursor = Query {
  query = "query { \
\    organization(login:\"" <> org <> "\") { \
\      repositories(first: " <> tshow f <> " " <> after <>  ") { \
\       nodes { \
\           isFork \
\           name \
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
      Just c -> "after: \"" <> c <> "\""

type ClientM a = ReaderT Config IO a

graphQL :: Query -> ClientM RepoResult
graphQL q = do
  c <- ask
  let t = encodeUtf8 $ githubToken c
      ep = https "api.github.com" /: "graphql"
      body = ReqBodyJson q
      prms =
        mconcat
          [ header "Authorization" ("bearer " <> t),
            header "User-Agent" "agent"
          ]
      request = req POST ep body jsonResponse prms
  responseBody <$> runReq defaultHttpConfig request

repos :: Text -> ClientM [Repository]
repos org = go org 100 Nothing
  where go o f a = do
          res <- graphQL $ orgRepos o f a
          let rs = getRepos res
          let page = getPageInfo res
          if hasNextPage page then
            (rs ++) <$> go o f (Just (endCursor page))
          else
            pure rs
  
runRepos :: Text -> IO [Repository]
runRepos org = do
  file <- defaultConfigFile
  cfg <- readConfig file
  case cfg of
    Left _ -> pure []
    Right c -> runReaderT (repos org) c
