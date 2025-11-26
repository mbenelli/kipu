{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kipu.GitHub.GraphQL.Types where

import           BasicPrelude
import           Data.Aeson
import           Data.Time        (UTCTime)
import           GHC.Generics     (Generic)
import           Kipu.JsonOptions (options)

data PageInfo = PageInfo
  { endCursor       :: Text
  , startCursor     :: Text
  , hasNextPage     :: Bool
  , hasPreviousPage :: Bool
  } deriving (Show, Generic, FromJSON, ToJSON)

data Repository = Repository
  { archivedAt     :: Maybe UTCTime
  , createdAt      :: UTCTime
  , isArchived     :: Bool
  , isDisabled     :: Bool
  , isEmpty        :: Bool
  , isFork         :: Bool
  , isLocked       :: Bool
  , isMirror       :: Bool
  , isPrivate      :: Bool
  , isTemplate     :: Bool
  , name           :: Text
  , stargazerCount :: Int
  , updatedAt      :: UTCTime
  , url            :: Text
  } deriving (Show, Generic, FromJSON, ToJSON)

data Repositories = Repositories
  { nodes    :: [Repository]
  , pageInfo :: PageInfo
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype Organization = Organization
  { repositories :: Repositories
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype RepoData = RepoData
  { organization :: Organization
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype RepoResult = RepoResult
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
