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

import qualified Lupo.Entry as DB
import qualified Lupo.Navigation as N

navigationSpec :: Spec
navigationSpec = describe "page navigation" $ do
  it "gets the next day" $ do
    let db = def
          { DB.afterSavedDays = \day -> checkArgument day (Time.fromGregorian 2000 1 2) $
              E.enumList 1 [Time.fromGregorian 2000 1 2]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    N.getNextDay nav `shouldReturn` Just (Time.fromGregorian 2000 1 2)

  it "gets the previous day" $ do
    let db = def
          { DB.beforeSavedDays = \day -> checkArgument day (Time.fromGregorian 1999 12 31) $
              E.enumList 1 [Time.fromGregorian 1999 12 31]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    N.getPreviousDay nav `shouldReturn` Just (Time.fromGregorian 1999 12 31)

  it "gets this month" $ do
    let nav = N.makeNavigation def $ Time.fromGregorian 2000 1 15 :: N.Navigation IO
    N.getThisMonth nav `shouldBe` Time.fromGregorian 2000 1 1

  it "gets top day of the next page" $ do
    let db = def
          { DB.afterSavedDays = \day -> checkArgument day (Time.fromGregorian 2000 1 2) $
              E.enumList 1
                [ Time.fromGregorian 2000 1 1
                , Time.fromGregorian 2000 1 2
                , Time.fromGregorian 2000 1 3
                ]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    N.getNextPageTop nav 3 `shouldReturn` Just (Time.fromGregorian 2000 1 3)

  it "gets top day of the previous page" $ do
    let db = def
          { DB.beforeSavedDays = \day -> checkArgument day (Time.fromGregorian 1999 12 31) $
              E.enumList 1
                [ Time.fromGregorian 1999 12 31
                , Time.fromGregorian 1999 12 30
                , Time.fromGregorian 1999 12 29
                ]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    N.getPreviousPageTop nav 3 `shouldReturn` Just (Time.fromGregorian 1999 12 29)

  it "gets the next month" $ do
    let db = def {DB.afterSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 2 1]}
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 15
    N.getNextMonth nav `shouldReturn` Just (Time.fromGregorian 2000 2 1)

  it "gets the previous month" $ do
    let db = def {DB.beforeSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 12 31]}
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 15
    N.getPreviousMonth nav `shouldReturn` Just (Time.fromGregorian 1999 12 1)

  it "gets the no monthes when there are no entries" $ do
    let db = def
          { DB.afterSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 1 5]
          , DB.beforeSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 1 3]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 4
    N.getNextMonth nav `shouldReturn` Nothing
    N.getPreviousMonth nav `shouldReturn` Nothing

checkArgument :: (Show a, Eq a) => a -> a -> b -> b
checkArgument actual expected value
  | expected == actual = value
  | otherwise = error $ "Argument must be: " <> show expected

instance (Functor m, Applicative m, Monad m) => Default (DB.EntryDatabase m) where
  def = DB.EntryDatabase
    { DB.selectOne = undefined
    , DB.selectPage = undefined
    , DB.selectAll = undefined
    , DB.search = undefined
    , DB.insert = undefined
    , DB.update = undefined
    , DB.delete = undefined
    , DB.beforeSavedDays = undefined
    , DB.afterSavedDays = undefined
    , DB.insertComment = undefined
    }
