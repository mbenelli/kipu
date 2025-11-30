{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Kipu.Jira.ApiTypes where

import           BasicPrelude
import           Data.Aeson
import           GHC.Generics
import           Kipu.Jira.CustomTypes  (IssueBean, IssueCoreObject)
import qualified Kipu.Jira.InsightTypes as JI
import           Kipu.JsonOptions       (options)


-- Deprecated API
--
data SearchRequestV2 = SearchRequestV2
  { searchRequestV2_expand        :: [Text]
  , searchRequestV2_fields        :: [Text]
  , searchRequestV2_jql           :: Text
  , searchRequestV2_maxResults    :: Int
  , searchRequestV2_startAt       :: Int
  , searchRequestV2_validateQuery :: Bool
  } deriving (Show, Generic)

instance FromJSON SearchRequestV2 where
  parseJSON = genericParseJSON options

instance ToJSON SearchRequestV2 where
  toJSON = genericToJSON options

data SearchResponseV2 = SearchResponseV2
  { searchResponseV2_expand         :: Maybe Text
  , searchResponseV2_issues         :: [IssueBean]
  , searchResponseV2_maxResults     :: Int
  , searchResponseV2_names          :: Maybe Object
  , searchResponseV2_schema         :: Maybe Object
  , searchResponseV2_startAt        :: Int
  , searchResponseV2_total          :: Int
  , searchResponseV2_warningMessage :: Maybe [Text]
  } deriving (Show, Generic)

instance FromJSON SearchResponseV2 where
  parseJSON = genericParseJSON options

instance ToJSON SearchResponseV2 where
  toJSON = genericToJSON options

--

data SearchRequest = SearchRequest
  { searchRequest_expand        :: Maybe Text
  , searchRequest_fields        :: Maybe [Text]
  , searchRequest_fieldsByKeys  :: Maybe Bool
  , searchRequest_jql           :: Text
  , searchRequest_maxResults    :: Maybe Int
  , searchRequest_nextPageToken :: Maybe Text
  , searchRequest_properties    :: Maybe [Text]
  , reconcileIssues             :: Maybe [Int]
  } deriving (Show, Generic)

instance FromJSON SearchRequest where
  parseJSON = genericParseJSON options

instance ToJSON SearchRequest where
  toJSON = genericToJSON options

searchRequest :: Text -> Maybe Text -> SearchRequest
searchRequest q t =
  SearchRequest Nothing Nothing Nothing q Nothing t Nothing Nothing

data SearchResponse = SearchResponse
  { searchResponse_isLast        :: Bool
  , searchResponse_issues        :: [IssueBean]
  , searchResponse_names         :: Maybe Object
  , searchResponse_nextPageToken :: Maybe Text
  , searchResponse_schema        :: Maybe Object
  } deriving (Show, Generic)

instance FromJSON SearchResponse where
  parseJSON = genericParseJSON options

instance ToJSON SearchResponse where
  toJSON = genericToJSON options

newtype CreateIssueRequest = CreateIssueRequest
  { createIssueRequest_fields :: IssueCoreObject
  }
  deriving (Show, Generic)

instance ToJSON CreateIssueRequest where
  toJSON = genericToJSON options

type AssetSearchResponse = Object

data AssetSearchResponse' = AssetSearchResponse'
  { asset_id                   :: Maybe Text
  , asset_title                :: Maybe Text
  , asset_type                 :: Maybe Text
  , asset_properties           :: Maybe Object
  , asset_definitions          :: Maybe Object
  , asset_additionalProperties :: Maybe Bool
  , asset_required             :: Maybe Object
  }
  deriving (Show, Generic)

instance FromJSON AssetSearchResponse' where
  parseJSON = genericParseJSON options

data InsightSearchResponse = InsightSearchResponse
  { insightSearchResponse_objectEntries        :: [JI.ObjectEntry]
  , insightSearchResponse_objectTypeAttributes :: [Maybe Object]
  , insightSearchResponse_iql                  :: Maybe Text
  , insightSearchResponse_qlQuery              :: Maybe Text
  , insightSearchResponse_startIndex           :: Int
  , insightSearchResponse_toIndex              :: Int
  , insightSearchResponse_totalFilterCount     :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON InsightSearchResponse where
  parseJSON = genericParseJSON options
