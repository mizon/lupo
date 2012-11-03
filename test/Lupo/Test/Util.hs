{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lupo.Test.Util
    ( utilTest
    ) where

import Control.Applicative
import qualified Data.Text as T
import qualified Data.Time as Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit hiding (Test)
import Test.QuickCheck

import qualified Lupo.Util as U

instance Arbitrary Time.ZonedTime where
    arbitrary = do
        utc <- arbitrary
        pure $ Time.utcToZonedTime someTimeZone utc
      where
        someTimeZone = Time.minutesToTimeZone 0

instance Arbitrary Time.UTCTime where
    arbitrary = do
        d <- arbitrary
        pure $ Time.UTCTime d someDiffTime
      where
        someDiffTime = Time.secondsToDiffTime 0

instance Arbitrary Time.Day where
    arbitrary = do
        y <- elements [2000..2020]
        m <- elements [1..12]
        d <- elements [1..30]
        pure $ Time.fromGregorian y m d

utilTest :: Test
utilTest = testGroup "utilities"
    [ testProperty "safeLast exist" $ \lst ->
        U.safeLast ([1, 2, lst] :: [Integer]) == Just lst

    , testCase "safeLast empty" $
        U.safeLast ([] :: [Integer]) @?= Nothing

    , testProperty "zonedDay" $ \zoned ->
        U.zonedDay zoned == Time.localDay (Time.zonedTimeToLocalTime zoned)

    , testProperty "toText" $ \(v :: Integer) ->
        U.toText v == T.pack (show v)
    ]
