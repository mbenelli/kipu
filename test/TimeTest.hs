{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: TimeTest
-- Description: Test time utilities
-- Copyright: (c) Marco Benelli 2024
-- License: ISC
-- Maintainer: mbenelli@fastmail.com
--
module TimeTest where

import           BasicPrelude
import           Data.Time          (UTCTime)
import           Data.Time.Calendar (fromGregorian)
import qualified Data.Time.Clock    as C
import           Kipu.Time
import           Test.HUnit


-- Sample Timestamps

t0 :: UTCTime
t0 = C.UTCTime (fromGregorian 2024 1 29) 3600

t1 :: UTCTime
t1 = C.UTCTime (fromGregorian 2024 1 29) 7200

t2 :: UTCTime
t2 = C.UTCTime (fromGregorian 2024 1 29) 10800

t3 :: UTCTime
t3 = C.UTCTime (fromGregorian 2024 1 30) 3600

t4 :: UTCTime
t4 = C.UTCTime (fromGregorian 2024 1 30) 7200

t5 :: UTCTime
t5 = C.UTCTime (fromGregorian 2024 1 30) 10800

t6 :: UTCTime
t6 = C.UTCTime (fromGregorian 2024 1 31) 3600

t7 :: UTCTime
t7 = C.UTCTime (fromGregorian 2024 1 31) 7200

t8 :: UTCTime
t8 = C.UTCTime (fromGregorian 2024 1 31) 10800

--

timeTest :: [Test]
timeTest =
  [ TestLabel
      "TimeInterval creation"
      ( TestCase $ do
          let dt0 = timeInterval t0 t1
          let dt1 = timeInterval t1 t0
          assertEqual "" dt0 dt1
          assertEqual "" (begin dt0) t0
          assertEqual "" (begin dt1) t0
          assertEqual "" (end dt0) t1
          assertEqual "" (end dt1) t1
      ),
    TestLabel
      "Intersection"
      ( TestCase $ do
          let i1 = timeInterval t0 t2
          let i2 = timeInterval t1 t3
          let int = timeInterval t1 t2
          case intersection i1 i2 of
            Nothing -> assertFailure "Intersection should not be empty"
            Just ii -> assertEqual "" ii int
      ),
    TestLabel
      "Intersections"
      ( TestCase $ do
          let i0 = timeInterval t0 t1
          let i1 = timeInterval t6 t8
          let i2 = timeInterval t0 t2
          let i3 = timeInterval t5 t7
          let xs = [i0, i1]
          let ys = [i2, i3]
          let ii = intersection' xs ys
          let ii0 = timeInterval t0 t1
          let ii1 = timeInterval t6 t7
          assertEqual "" ii [ii0, ii1]
      )
  ]
