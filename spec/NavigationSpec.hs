{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module NavigationSpec
  ( navigationSpec
  ) where

import Control.Applicative
import Control.Monad.Writer
import Data.Default
import qualified Data.Enumerator as E
import qualified Data.Time as Time
import Test.Hspec

import qualified Lupo.Database as DB
import qualified Lupo.Navigation as N

navigationSpec :: Spec
navigationSpec = describe "page navigation" $ do
  it "gets the next day" $ do
    let db = def
          { DB.afterSavedDays = \day -> do
              tell [day]
              pure $ E.enumList 1 [Time.fromGregorian 2000 1 2]
          }
    (day, arg) <- runWriterT $ do
      let nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
      N.getNextDay nav
    day `shouldBe` Just (Time.fromGregorian 2000 1 2)
    arg `shouldBe` [Time.fromGregorian 2000 1 2]

  it "gets the previous day" $ do
    let db = def
          { DB.beforeSavedDays = \day -> do
              tell [day]
              pure $ E.enumList 1 [Time.fromGregorian 1999 12 31]
          }
    (day, arg) <- runWriterT $ do
      let nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
      N.getPreviousDay nav
    day `shouldBe` Just (Time.fromGregorian 1999 12 31)
    arg `shouldBe` [Time.fromGregorian 1999 12 31]

  it "gets this month" $ do
    let nav = N.makeNavigation def $ Time.fromGregorian 2000 1 15 :: N.Navigation IO
    N.getThisMonth nav `shouldBe` Time.fromGregorian 2000 1 1

  it "gets top day of the next page" $ do
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
    day `shouldBe` Just (Time.fromGregorian 2000 1 3)
    arg `shouldBe` [Time.fromGregorian 2000 1 2]

  it "gets top day of the previous page" $ do
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
    day `shouldBe` Just (Time.fromGregorian 1999 12 29)
    arg `shouldBe` [Time.fromGregorian 1999 12 31]

  it "gets the next month" $ do
    let db = def {DB.afterSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 2000 2 1]}
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 15
    N.getNextMonth nav `shouldReturn` Just (Time.fromGregorian 2000 2 1)

  it "gets the previous month" $ do
    let db = def {DB.beforeSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 2000 12 31]}
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 15
    N.getPreviousMonth nav `shouldReturn` Just (Time.fromGregorian 1999 12 1)

  it "gets the no monthes when there are no entries" $ do
    let db = def
          { DB.afterSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 2000 1 5]
          , DB.beforeSavedDays = const $ pure $ E.enumList 1 [Time.fromGregorian 2000 1 3]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 4
    N.getNextMonth nav `shouldReturn` Nothing
    N.getPreviousMonth nav `shouldReturn` Nothing

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
    , DB.insertComment = undefined
    }
