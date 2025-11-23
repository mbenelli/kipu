{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.GitHub.Types
-- Description: GitHub types
-- Copyright: (c) Marco Benelli 2025
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
module Kipu.GitHub.Types where

import           BasicPrelude
import           Data.Aeson
import           GHC.Generics     (Generic)
import           Kipu.JsonOptions (options)

data User = User
  { user_login               :: Text
--  , user_id                  :: Int
--  , user_node_id             :: Text
  , user_avatar_url          :: Text -- Url
--  , user_gravatar_id         :: Text
  , user_url                 :: Text -- Url
  , user_html_url            :: Text -- Url
  , user_followers_url       :: Text -- Url
  , user_following_url       :: Text -- Url
  , user_gists_url           :: Text -- Url
  , user_starred_url         :: Text -- Url
  , user_subscriptions_url   :: Text -- Url
  , user_organizations_url   :: Text -- Url
  , user_repos_url           :: Text -- Url
  , user_events_url          :: Text -- Url
  , user_received_events_url :: Text -- Url
  , user_type                :: Text -- UserType
  , user_site_admin          :: Bool
  } deriving (Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON options

instance ToJSON User where
  toJSON = genericToJSON options

data Permissions = Permissions
  { permissions_admin    :: Maybe Bool
  , permissions_maintain :: Maybe Bool
  , permissions_push     :: Maybe Bool
  , premission_triage    :: Maybe Bool
  , permissions_pull     :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON Permissions where
  parseJSON = genericParseJSON options

instance ToJSON Permissions where
  toJSON = genericToJSON options

data Repository = Repository
  { repository_id                :: Int
  , repository_name              :: Text
  , repository_full_name         :: Text
--  , repository_owner             :: User
  , repository_private           :: Bool
  , repository_html_url          :: Text -- Url
  , repository_description       :: Maybe Text
  , repository_fork              :: Bool
  , repository_url               :: Text -- Url
  , repository_language          :: Maybe Text
  , repository_forks_count       :: Int
  , repository_stargazer_count   :: Maybe Int
  , repository_watchers_count    :: Int
  , repository_size              :: Int
  , repository_default_branch    :: Text
  , repository_open_issues_count :: Int
  , repository_is_template       :: Bool
  , repository_topics            :: Maybe [Text]
  , repository_has_issues        :: Bool
  , repository_has_projects      :: Bool
  , repository_has_wiki          :: Bool
  , repository_has_pages         :: Bool
  , repository_has_downloads     :: Bool
  , repository_has_discussions   :: Bool
  , repository_archived          :: Bool
  , repository_disabled          :: Bool
  , repository_visibility        :: Text -- Visibility
  , repository_pushed_at         :: Maybe Text -- Datetime
  , repository_created_at        :: Maybe Text -- Datetime
  , repository_updated_at        :: Maybe Text -- Datetime
  , repository_permissions       :: Permissions
  } deriving (Show, Generic)

instance FromJSON Repository where
  parseJSON = genericParseJSON options

instance ToJSON Repository where
  toJSON = genericToJSON options
