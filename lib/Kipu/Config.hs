{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData        #-}

{-|
Module: Kipu.Config
Description: Kipu Configuration
Copyright: (c) Marco Benelli 2024
License: ISC
Maintainer: mbenelli@fastmail.com

Read parameters from a configuration file and make them available
as a data structure.

The path of the file is currently fixed, and located in the
XdgConfig directory.
-}
module Kipu.Config where

import           BasicPrelude
import           Data.Yaml        (FromJSON, ParseException, decodeFileEither)
import           GHC.Generics
import           System.Directory

data Config = Config
  { url           :: Text
  , user          :: Text
  , authorization :: Text
  , token         :: Text
  , githubToken   :: Text
  , crtPath       :: Maybe Text
  , keyPath       :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance FromJSON Config

auth :: Config -> Text
auth c = unwords [authorization c, token c]

defaultConfigFile :: IO String
defaultConfigFile = getXdgDirectory XdgConfig "kipu/config.yaml"

readConfig :: String -> IO (Either ParseException Config)
readConfig = decodeFileEither
