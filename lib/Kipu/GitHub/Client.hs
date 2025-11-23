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
module Kipu.GitHub.Client where

import           BasicPrelude
import           Data.Aeson
import           Data.ByteString.Char8 (pack)
import           GHC.Generics          (Generic)
import           Kipu.Config
import           Kipu.GitHub.Types     (Repository)
import           Network.HTTP.Req

newtype Query = Query
  { query :: Text
  }
  deriving (Show, Generic, FromJSON, ToJSON)

type Token = ByteString

queryBs :: ByteString
queryBs = "{\"query\": \"query { viewer { login }}\"}"

login :: Query
login = Query {query = "query {viewer {login}}"}

schema :: Query
schema = Query {query = "query { __schema { types { name kind description fields { name }}}}"}

typeList :: Query
typeList = Query {query = "query {__schema { types { name }}}"}

typeInfo :: Text -> Query
typeInfo t = Query {query = "query { __type(name: \"" <> t <> "\") { name kind description fields { name }}}"}

organization :: Query
organization = typeInfo "Organization"

orgInfo :: Text -> Query
orgInfo n = Query {
  query =
    "query { organization(login: \"" <> n <> "\") "
    <> "{ archivedAt createdAt description email id login"
    <> " membersWithRole(first: 10) {totalCount edges { node { name }}}"
    <> " name"
    <> " packages(first: 10) {totalCount}"
--    <> " projectV2(first: 10) { edges {node { name }}}"
--    <> " projectsUrl(first: 10) {totalCount}"
    <> " repositories(first: 100) { totalCount nodes { name } }"
--    <> " team(first: 10) {}"
    <> " teamsUrl"
    <> " url"
    <> "}}"
}

repositoryConnection :: Query
repositoryConnection = typeInfo "RepositoryConnection"

repository :: Query
repository = typeInfo "Repository"

mbrepos :: Query
mbrepos = Query {
  query = "query { search(query:\"owner:mercedes-benz\" type: REPOSITORY first: 5 after: null) {repositoryCount}}"
}

mbrepo :: Query
mbrepo = Query {
  query = "query { search(query:\"name:DnA forks:>20\" type: REPOSITORY first: 5 after: null) {repositoryCount nodes { ... on Repository {name }}}}"
}

user :: Query
user = typeInfo "User"

pullRequest :: Query
pullRequest = typeInfo "PullRequest"

graphql :: Query -> Token -> IO ByteString
graphql q t = do
  let ep = https "api.github.com" /: "graphql"
      body = ReqBodyJson q
      prms =
        mconcat
          [ header "Authorization" ("bearer " <> t),
            header "User-Agent" "agent"
          ]
      request = req POST ep body bsResponse prms
  responseBody <$> runReq defaultHttpConfig request

runGraphql :: Query -> IO ByteString
runGraphql q = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left e    -> return $ pack $ show e
    Right cfg -> graphql q (encodeUtf8 $ githubToken cfg)

type Endpoint = Text

repos :: Text -> Token -> IO [Repository]
repos org ghToken = do
  let ep = https "api.github.com" /: "orgs" /: org /: "repos"
      prms =
        mconcat
          [ header "Authorization" ("bearer " <> ghToken),
            header "User-Agent" "agent",
            header "Accept" "application/vnd.github+json",
            header "X-GitHub-Api-Version" "2022-11-28"
          ]
      body = NoReqBody
      request = req GET ep body jsonResponse prms
  responseBody <$> runReq defaultHttpConfig request

run :: Text -> IO [Repository]
run org = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left e    -> pure [] -- return $ pack $ show e
    Right cfg -> repos org (encodeUtf8 $ githubToken cfg)

