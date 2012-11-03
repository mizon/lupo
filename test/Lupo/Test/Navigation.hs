{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.Test.Navigation
    ( navigationTest
    ) where

import Control.Applicative
import Control.Monad.Reader
import qualified Data.Enumerator as E
import qualified Data.Time as Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Lupo.Database as DB
import qualified Lupo.Navigation as N

navigationTest :: Test
navigationTest = testGroup "page navigation"
    [ testCase "getNextDay" $ do
        next <- withDBMock makeNextDaysMock $ do
            nav <- N.initNavigation <$> ask <*> pure (Time.fromGregorian 2000 1 1)
            N.getNextDay nav
        next @?= Just (Time.fromGregorian 2000 2 2)

    , testCase "getPreviousDay" $ do
        previous <- withDBMock makePreviousDaysMock $ do
            nav <- N.initNavigation <$> ask <*> pure (Time.fromGregorian 2000 1 1)
            N.getPreviousDay nav
        previous @?= Just (Time.fromGregorian 1999 12 1)

    , testCase "getThisMonth" $ do
        let (N.getThisMonth -> month) =
                N.initNavigation mockedDB $ Time.fromGregorian 2000 1 1 :: N.Navigation IO
        month @?= Time.fromGregorian 2000 1 1

    -- , testCase "getNextPageTop" $ do


    -- , testCase "getPreviousPageTop"

    -- , testCase "getNextMonth"

    -- , testCase "getPreviousMonth"
    ]
  where
    makeNextDaysMock = mockedDB
        { DB.afterSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 2000 2 2]
        }

    makePreviousDaysMock = mockedDB
        { DB.beforeSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 1999 12 1]
        }

    withDBMock :: DB.Database -> ReaderT DB.Database IO a -> IO a
    withDBMock mock = flip runReaderT mock

mockedDB :: DB.Database
mockedDB = DB.Database
    { DB.select = undefined
    , DB.selectDay = undefined
    , DB.all = undefined
    , DB.search = undefined
    , DB.insert = undefined
    , DB.update = undefined
    , DB.delete = undefined
    , DB.beforeSavedDays = undefined
    , DB.afterSavedDays = undefined
    }
