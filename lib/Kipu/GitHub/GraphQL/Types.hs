{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Module: Kipu.GitHub.GraphQL.Types
-- Description: Data types for reading GraphQL responses
-- Copyright: (c) Marco Benelli 2025
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
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

type family ResultValue t

class FromJSON r => Paged r where
  pageInfo :: r -> PageInfo
  values :: r -> [ResultValue r]

-------------------------------------------------
-- Repository Components
--

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
  { repositories_nodes    :: [Repository]
  , repositories_pageInfo :: PageInfo
  } deriving (Show, Generic)

instance FromJSON Repositories where
  parseJSON = genericParseJSON options

instance ToJSON Repositories where
  toJSON = genericToJSON options

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

-- Repositories instance
--
type instance ResultValue RepoResult = Repository

instance Paged RepoResult where
  pageInfo = repositories_pageInfo . repositories . organization . repo_data
  values = repositories_nodes . repositories . organization . repo_data

-------------------------------------------------
-- Push Request Components

-- | Git Author
--
-- The date comes from  an ISO-8601 encoded date string
-- not converted in UTC. (according to GitHub documentation)
--
data GitAuthor = GitAuthor
  { gitAuthor_date  :: Maybe UTCTime
  , gitAuthor_email :: Maybe Text
  , gitAuthor_name  :: Maybe Text
  , gitAuthor_user  :: Maybe Actor
  } deriving (Show, Generic)

instance FromJSON GitAuthor where
  parseJSON = genericParseJSON options

instance ToJSON GitAuthor where
  toJSON = genericToJSON options

data GitAuthors = GitAuthors
  { gitAuthors_nodes      :: [GitAuthor]
  , gitAuthors_pageInfo   :: PageInfo
  , gitAuthors_totalCount :: Int
  } deriving (Show, Generic)

instance FromJSON GitAuthors where
  parseJSON = genericParseJSON options

instance ToJSON GitAuthors where
  toJSON = genericToJSON options

data Commit = Commit
  { commit_authors        :: GitAuthors
  , commit_abbreviatedOid :: Text
  } deriving (Show, Generic)

instance FromJSON Commit where
  parseJSON = genericParseJSON options

instance ToJSON Commit where
  toJSON = genericToJSON options

newtype PullRequestCommit = PullRequestCommit
  { pullRequestCommit_commit :: Commit
  } deriving (Show, Generic)

instance FromJSON PullRequestCommit where
  parseJSON = genericParseJSON options

instance ToJSON PullRequestCommit where
  toJSON = genericToJSON options

data Commits = Commits
  { commits_nodes      :: [PullRequestCommit]
  , commits_pageInfo   :: PageInfo
  , commits_totalCount :: Int
  } deriving (Show, Generic)

instance FromJSON Commits where
  parseJSON = genericParseJSON options

instance ToJSON Commits where
  toJSON = genericToJSON options

newtype Actor = Actor
  { actor_login :: Text
  } deriving (Show, Generic)

instance FromJSON Actor where
  parseJSON = genericParseJSON options

instance ToJSON Actor where
  toJSON = genericToJSON options

data PullRequest = PullRequest
  { pullRequest_author      :: Maybe Actor
  , pullRequest_closed      :: Bool
  , pullRequest_closedAt    :: Maybe UTCTime
  , pullRequest_commits     :: Commits
  , pullRequest_createdAt   :: UTCTime
  , pullRequest_id          :: Text
  , pullRequest_merged      :: Bool
  , pullRequest_mergedAt    :: Maybe UTCTime
  , pullRequest_number      :: Int
  , pullRequest_permalink   :: Text
  , pullRequest_publishedAt :: Maybe UTCTime
  , pullRequest_title       :: Text
  , pullRequest_updatedAt   :: UTCTime
  , pullRequest_url         :: Text
  } deriving (Show, Generic)

instance FromJSON PullRequest where
  parseJSON = genericParseJSON options

instance ToJSON PullRequest where
  toJSON = genericToJSON options

data PullRequests = PullRequests
  { pullRequests_nodes    :: [PullRequest]
  , pullRequests_pageInfo :: PageInfo
  } deriving (Show, Generic)

instance FromJSON PullRequests where
  parseJSON = genericParseJSON options

instance ToJSON PullRequests where
  toJSON = genericToJSON options

newtype PrRepo = PrRepo
  { pullRequests :: PullRequests
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype PrData = PrData
  { repository:: PrRepo
  } deriving (Show, Generic, FromJSON, ToJSON)

newtype PrResult = PrResult
  { pr_data :: PrData
  } deriving (Show, Generic)

instance FromJSON PrResult where
  parseJSON = genericParseJSON options

instance ToJSON PrResult where
  toJSON = genericToJSON options

type instance ResultValue PrResult = PullRequest

instance Paged PrResult where
  pageInfo = pullRequests_pageInfo . pullRequests . repository . pr_data
  values = pullRequests_nodes . pullRequests . repository . pr_data
