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

repository :: Query
repository = typeInfo "Repository"

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

run :: Query -> IO ByteString
run q = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left e    -> return $ pack $ show e
    Right cfg -> graphql q (encodeUtf8 $ githubToken cfg)
