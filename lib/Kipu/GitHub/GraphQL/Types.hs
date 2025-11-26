{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kipu.GitHub.GraphQL.Types where

import           BasicPrelude
import           Data.Aeson
import           GHC.Generics     (Generic)
import           Kipu.JsonOptions (options)

data PageInfo = PageInfo
  { endCursor :: Text
  , startCursor :: Text
  , hasNextPage :: Bool
  , hasPreviousPage :: Bool
  } deriving (Show, Generic, FromJSON, ToJSON)

-- data Repository = Repository
--   { archivedAt :: Maybe Text -- Date
--   , codeowners :: Maybe [Text]
--   , createdAt :: Text -- Date
--   , description :: Maybe Text
--   , forkCount :: Int
--   , isArchieved :: Bool
--   , isDisabled :: Bool
--   , isEmpty :: Bool
--   , isFork :: Bool
--   , isLocked :: Bool
--   , isMirror :: Bool
--   , isPrivate :: Bool
--   , isTemplate :: Bool
--   , name :: Text
--   , stargazerCount :: Int
--   , updatedAt :: Maybe Text -- Date
--   , url :: Text -- Url
--   } deriving (Show, Generic)

data Repository = Repository
  { isFork :: Bool
  , name :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data Repositories = Repositories
  { nodes :: [Repository]
  , pageInfo :: PageInfo
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype Organization = Organization
  { repositories :: Repositories
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype RepoData = RepoData
  { organization :: Organization
  } deriving (Show, Generic, FromJSON, ToJSON)

data RepoResult = RepoResult
  { repo_data :: RepoData
  } deriving (Show, Generic)

instance FromJSON RepoResult where
  parseJSON = genericParseJSON options

instance ToJSON RepoResult where
  toJSON = genericToJSON options


getRepos :: RepoResult -> [Repository]
getRepos = nodes . repositories . organization . repo_data

getPageInfo:: RepoResult -> PageInfo
getPageInfo = pageInfo . repositories . organization . repo_data

nullResult :: RepoResult
nullResult = RepoResult {
  repo_data = RepoData {
    organization = Organization {
      repositories = Repositories {
        nodes = [],
        pageInfo = PageInfo {
          endCursor = "",
          startCursor = "",
          hasNextPage = False,
          hasPreviousPage = False }}}}}
