{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: TransformTest
-- Description: Test transformation
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
module TestUtils where

import           BasicPrelude
import           Data.Aeson           (FromJSON, eitherDecode)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T

fromFile :: (FromJSON a) => Text -> IO (Either String a)
fromFile f = eitherDecode <$> BL.readFile (T.unpack f)
