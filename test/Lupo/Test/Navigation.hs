{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lupo.Test.Navigation
  ( navigationTest
  ) where

import Control.Applicative
import Control.Monad.Writer
import Data.Default
import qualified Data.Enumerator as E
import qualified Data.Time as Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Lupo.Database as DB
import qualified Lupo.Navigation as N

instance (Functor m, Applicative m, Monad m) =>
    Default (DB.Database m) where
  def = DB.Database
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

navigationTest :: Test
navigationTest = testGroup "page navigation"
  [ testCase "getNextDay" $ do
      let db = def
            { DB.afterSavedDays = \day -> do
                tell [day]
                pure $ E.enumList 1 [Time.fromGregorian 2000 1 2]
            }
      (day, arg) <- runWriterT $ do
        let nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
        N.getNextDay nav
      day @?= Just (Time.fromGregorian 2000 1 2)
      arg @?= [Time.fromGregorian 2000 1 2]

  , testCase "getPreviousDay" $ do
      let db = def
            { DB.beforeSavedDays = \day -> do
                tell [day]
                pure $ E.enumList 1 [Time.fromGregorian 1999 12 31]
            }
      (day, arg) <- runWriterT $ do
        let nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
        N.getPreviousDay nav
      day @?= Just (Time.fromGregorian 1999 12 31)
      arg @?= [Time.fromGregorian 1999 12 31]

  , testCase "getThisMonth" $ do
      let nav = N.makeNavigation def $ Time.fromGregorian 2000 1 15 :: N.Navigation IO
      N.getThisMonth nav @?= Time.fromGregorian 2000 1 1

  , testCase "getNextPageTop" $ do
      let db = def
            { DB.afterSavedDays = \day -> do
                tell [day]
                pure $ E.enumList 1
                  [ Time.fromGregorian 2000 1 1
                  , Time.fromGregorian 2000 1 2
                  , Time.fromGregorian 2000 1 3
                  ]
            }
      (day, arg) <- runWriterT $ do
        let nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
        N.getNextPageTop nav 3
      day @?= Just (Time.fromGregorian 2000 1 3)
      arg @?= [Time.fromGregorian 2000 1 2]

  , testCase "getPreviousPageTop" $ do
      let db = def
            { DB.beforeSavedDays = \day -> do
                tell [day]
                pure $ E.enumList 1
                  [ Time.fromGregorian 1999 12 31
                  , Time.fromGregorian 1999 12 30
                  , Time.fromGregorian 1999 12 29
                  ]
            }
      (day, arg) <- runWriterT $ do
        let nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
        N.getPreviousPageTop nav 3
      day @?= Just (Time.fromGregorian 1999 12 29)
      arg @?= [Time.fromGregorian 1999 12 31]

  , testCase "getNextMonth" $ do
      let nav = N.makeNavigation def $ Time.fromGregorian 2000 1 15 :: N.Navigation IO
      month <- N.getNextMonth nav
      month @?= Time.fromGregorian 2000 2 1

  , testCase "getPreviousMonth" $ do
      let nav = N.makeNavigation def $ Time.fromGregorian 2000 1 15 :: N.Navigation IO
      month <- N.getPreviousMonth nav
      month @?= Time.fromGregorian 1999 12 1
  ]
