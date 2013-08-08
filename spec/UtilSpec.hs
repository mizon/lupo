{-# OPTIONS_GHC -fno-warn-orphans #-}

module UtilSpec
  ( utilSpec
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens.Getter
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time as Time
import System.Locale
import Test.Hspec
import Test.QuickCheck

import qualified Lupo.Util as U

utilSpec :: Spec
utilSpec = describe "utility functions" $ do
  it "gets zonedDay" $
    property $ \zoned ->
      U.zonedDay zoned == zoned ^. U.zonedTimeToLocalTime ^. U.localDay

  it "converts showable datas to texts" $
    property $ \(v :: Integer) ->
      U.toText v == T.pack (show v)

  it "gets an index safely" $
    property $ \(xs :: [Int]) (i :: Int) ->
      case U.safeIndex xs i of
        Just x -> x == xs !! i
        Nothing -> i < 0 || length xs <= i

  it "format zoned-time for atom feeds" $ do
    let t = fromMaybe (assert False undefined)
          $ Time.parseTime defaultTimeLocale "%FT%T%z" "2013-08-05T23:39:01+0900"
    U.formatTimeForAtom t `shouldBe` "2013-08-05T23:39:01+09:00"

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
    d <- elements [1..31]
    pure $ Time.fromGregorian y m d
