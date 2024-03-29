{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: IssueBeanTest
-- Description: Test reading and parsing sample 'IssueBean's
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
module IssueBeanTest where

import qualified Kipu.Jira.CustomTypes as CT
import qualified Kipu.Utils as U
import Test.HUnit

issueBeanTest :: [Test]
issueBeanTest =
  [ TestLabel
      "IssueBean"
      ( TestCase $ do
          i :: Either String CT.IssueBean <- U.fromFile "test/samples/issuebean.json"
          case i of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> assertEqual "" (CT.issueBean_key x) "OR-3"
      ),
    TestLabel
      "Beans"
      ( TestCase $ do
          is :: Either String [CT.IssueBean] <- U.fromFile "test/samples/issuebeans.json"
          case is of
            Left s -> assertFailure $ "Error: " ++ show s
            Right x -> assertEqual "" (length x) 3
      )
  ]
