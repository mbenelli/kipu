{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module: TransformTest
Description: Test transformation
Copyright: (c) Marco Benelli 2024
License: ISC
Maintainer: mbenelli@fastmail.com
-}
module TransformTest where

import           BasicPrelude
import qualified Data.Map              as M
import           Data.Time             (UTCTime, addUTCTime, getCurrentTime,
                                        nominalDay)
import qualified Kipu.Jira.CustomTypes as CT
import           Kipu.Time             (TimeInterval (..), parseTime)
import           Kipu.Transform
import qualified Kipu.Types            as KT
import           Test.HUnit
import qualified TestUtils             as U

mkChange :: UTCTime -> Text -> Text -> Text -> KT.Change
mkChange ts f from to =
  KT.Change
    { KT.change_timestamp = ts
    , KT.change_author = KT.User "foo"
    , KT.change_field = f
    , KT.change_type = "jira"
    , KT.change_from = ""
    , KT.change_fromString = from
    , KT.change_to = ""
    , KT.change_toString = to
    }

sampleChanges01 :: [KT.Change]
sampleChanges01 =
  [ mkChange t0 "status" "To Do" "In Progress"
  , mkChange t1 "status" "In Progress" "Blocked"
  , mkChange t2 "status" "Blocked" "In Progress"
  , mkChange t3 "status" "In Progress" "Done"
  ]
  where
    t0 : t1 : t2 : t3 : _ = expectedTimestamps

statusSequence01 :: [KT.Status]
statusSequence01 = map KT.Status ["To Do", "In Progress", "Blocked", "In Progress", "Done"]


changes :: IO [KT.Change]
changes = do
  is :: Either String [CT.IssueBean] <-
    U.fromFile
      "test/samples/issuebean-changelog.json"
  case is of
    Left s -> assertFailure $ "Error: " ++ show s
    Right [] -> assertFailure "Unexpected empty [IssueBean]"
    Right (x : _) -> case KT.getChanges x of
      Nothing -> assertFailure "Error: cannot get changes from issue"
      Just cs -> return cs

time0 :: UTCTime
time0 = addUTCTime (-nominalDay) (head expectedTimestamps)

expectedTimestamps :: [UTCTime]
expectedTimestamps =
  mapMaybe
    parseTime
    [ "2024-01-29T09:04:34.533+0000"
    , "2024-01-29T09:04:59.866+0000"
    , "2024-01-29T09:08:06.018+0000"
    , "2024-01-30T08:46:38.765+0000"
    , "2024-01-30T08:46:41.194+0000"
    , "2024-01-30T08:46:44.160+0000"
    , "2024-01-30T08:46:47.685+0000"
    , "2024-02-01T22:22:22.326+0000"
    , "2024-02-01T22:23:48.835+0000"
    , "2024-02-19T07:45:20.069+0000"
    , "2024-02-19T09:07:02.766+0000"
    , "2024-02-19T09:07:06.330+0000"
    ]

expectedStates :: M.Map UTCTime KT.Status
expectedStates =
  M.fromList
    [ (t0, KT.Status "In Progress")
    , (t1, KT.Status "Blocked")
    , (t2, KT.Status "In Progress")
    , (t3, KT.Status "Done")
    ]
  where
    t0 : t1 : t2 : t3 : _ = expectedTimestamps

expectedHistory :: [(UTCTime, KT.Status)]
expectedHistory = zip (time0 : expectedTimestamps) statusSequence01

expectedIntervals :: IO [(TimeInterval, KT.Status)]
expectedIntervals = do
  t <- getCurrentTime
  return
    $ zip
      ( zipWith
          TimeInterval
          (time0 : expectedTimestamps)
          (expectedTimestamps ++ [t])
      )
      statusSequence01

transformTest :: [Test]
transformTest =
  [ TestLabel
      "Changes timestamps"
      ( TestCase $ do
          cs <- changes
          assertEqual "" (map KT.change_timestamp cs) expectedTimestamps
      ),
    TestLabel
      "Status changes"
      ( TestCase $ do
          assertEqual "" (issueStates sampleChanges01) expectedStates
      ),
    TestLabel
      "history"
      ( TestCase $ do
          assertEqual
            ""
            expectedHistory
            $ history
              time0
              (\x -> KT.change_field x == "status")
              KT.Status
              sampleChanges01
      ),
    TestLabel
      "intervals"
      ( TestCase $ do
          i <- intervals expectedHistory
          e <- expectedIntervals
          assertEqual "" (take 4 e) (take 4 i)
      ),
    TestLabel
      "intervals'"
      ( TestCase $ do
          e <- expectedIntervals
          assertEqual "" (take 4 e) $ intervals' expectedHistory
      )
  ]
