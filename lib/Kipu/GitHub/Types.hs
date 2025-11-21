{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- |
-- Module: Kipu.GitHub.Types
-- Description: GitHub types
-- Copyright: (c) Marco Benelli 2025
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module Kipu.GitHub.Types where

import BasicPrelude
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

data Foo = Foo
  {
  }
  deriving (Show, Generic)
