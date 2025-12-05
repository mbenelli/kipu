{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Kipu.GitHub.Rest.Client
-- Description: Client for GitHub REST API
-- Copyright: (c) Marco Benelli 2025
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
module Kipu.GitHub.Rest.Client where

import           BasicPrelude
import           Kipu.Config
import           Kipu.GitHub.Rest.Types
import           Network.HTTP.Req

type Endpoint = Text

repos :: Text -> ByteString-> IO [Repository]
repos org ghToken = do
  let ep = https "api.github.com" /: "orgs" /: org /: "repos"
      prms =
        mconcat
          [ header "Authorization" ("bearer " <> ghToken),
            header "User-Agent" "agent",
            header "Accept" "application/vnd.github+json",
            header "X-GitHub-Api-Version" "2022-11-28"
          ]
      body = NoReqBody
      request = req GET ep body jsonResponse prms
  responseBody <$> runReq defaultHttpConfig request

run :: Text -> IO [Repository]
run org = do
  file <- defaultConfigFile
  c <- readConfig file
  case c of
    Left _    -> pure [] -- return $ pack $ show e
    Right cfg -> repos org (encodeUtf8 $ githubToken cfg)

