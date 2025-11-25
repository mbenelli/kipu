{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Kipu.JsonOptions where

import BasicPrelude
import Data.Aeson (defaultOptions, fieldLabelModifier, Options)
import Data.Text as T

fieldModifier :: String -> String
-- fieldModifier = unpack . last . splitOn "_" . pack
fieldModifier = T.unpack . T.drop 1 . T.dropWhile (/='_') . T.pack

options :: Options
options = defaultOptions {fieldLabelModifier = fieldModifier}
