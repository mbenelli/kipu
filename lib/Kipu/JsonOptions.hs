{-# LANGUAGE NoImplicitPrelude #-}

{-|
Module: Kipu.JsonOptions
Description: Options for serializing/deserializing Json
Copyright: (c) Marco Benelli 2025
License: ISC
Maintainer: mbenelli@fastmail.com
-}
module Kipu.JsonOptions where

import           BasicPrelude
import           Data.Aeson   (Options, defaultOptions, fieldLabelModifier,
                               omitNothingFields)
import qualified Data.Text    as T

fieldModifier :: String -> String
fieldModifier = T.unpack . T.drop 1 . T.dropWhile (/='_') . T.pack

options :: Options
options = defaultOptions {
    fieldLabelModifier = fieldModifier
  , omitNothingFields = True
  }
