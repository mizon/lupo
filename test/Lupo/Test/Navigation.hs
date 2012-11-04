{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.Test.Navigation
  ( navigationTest
  ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Reader
import qualified Data.Enumerator as E
import qualified Data.Time as Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Lupo.Database as DB
import qualified Lupo.Navigation as N

newtype TestEnv a = TestEnv
  { unTestEnv :: ReaderT (DB.Database TestEnv) IO a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadCatchIO
    , MonadReader (DB.Database TestEnv)
    )

navigationTest :: Test
navigationTest = testGroup "page navigation"
  [ testCase "getNextDay" $ do
      next <- withDBMock mockedDB
          { DB.afterSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 2000 2 2]
          } $ do
        nav <- N.initNavigation <$> ask <*> pure (Time.fromGregorian 2000 1 1)
        N.getNextDay nav
      next @?= Just (Time.fromGregorian 2000 2 2)

  , testCase "getPreviousDay" $ do
      previous <- withDBMock mockedDB
          { DB.beforeSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 1999 12 1]
          } $ do
        nav <- N.initNavigation <$> ask <*> pure (Time.fromGregorian 2000 1 1)
        N.getPreviousDay nav
      previous @?= Just (Time.fromGregorian 1999 12 1)

  , testCase "getThisMonth" $ do
      let (N.getThisMonth -> month) =
            N.initNavigation mockedDB $ Time.fromGregorian 2000 1 1 :: N.Navigation IO
      month @?= Time.fromGregorian 2000 1 1

  , testCase "getNextPageTop" $ do
      next <- withDBMock mockedDB
          { DB.afterSavedDays = const $ pure $ E.enumList 1
            [ Time.fromGregorian 2000 1 1
            , Time.fromGregorian 2000 1 2
            , Time.fromGregorian 2000 1 3
            ]
          } $ do
        nav <- N.initNavigation <$> ask <*> pure (Time.fromGregorian 2000 1 1)
        N.getNextPageTop nav 3
      next @?= Just (Time.fromGregorian 2000 1 3)

  , testCase "getPreviousPageTop" $ do
      previous <- withDBMock mockedDB
          { DB.beforeSavedDays = const $ pure $ E.enumList 1
            [ Time.fromGregorian 2000 12 31
            , Time.fromGregorian 2000 12 30
            , Time.fromGregorian 2000 12 29
            ]
          } $ do
        nav <- N.initNavigation <$> ask <*> pure (Time.fromGregorian 2000 1 1)
        N.getPreviousPageTop nav 3
      previous @?= Just (Time.fromGregorian 2000 12 29)

    -- , testCase "getNextMonth"

    -- , testCase "getPreviousMonth"
    ]
  where
    withDBMock :: DB.Database TestEnv -> TestEnv a -> IO a
    withDBMock mock (unTestEnv -> m) = runReaderT m mock

mockedDB :: (Functor m, Applicative m, Monad m) => DB.Database m
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
