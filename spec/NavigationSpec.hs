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

import qualified Lupo.Backends.Navigation as N
import qualified Lupo.Entry as DB
import Lupo.Import
import qualified Lupo.Navigation as N

navigationSpec :: Spec
navigationSpec = describe "page navigation" $ do
  it "gets the next day" $ do
    let db = def
          { DB._afterSavedDays = \day ->
              checkArgument day (Time.fromGregorian 2000 1 2) $
                E.enumList 1 [Time.fromGregorian 2000 1 2]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    nav ^! N.getNextDay `shouldReturn` Just (Time.fromGregorian 2000 1 2)

  it "gets the previous day" $ do
    let db = def
          { DB._beforeSavedDays = \day ->
              checkArgument day (Time.fromGregorian 1999 12 31) $
                E.enumList 1 [Time.fromGregorian 1999 12 31]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    nav ^! N.getPreviousDay `shouldReturn` Just (Time.fromGregorian 1999 12 31)

  it "gets this month" $ do
    let nav = N.makeNavigation def $ Time.fromGregorian 2000 1 15 :: N.Navigation IO
    nav ^. N.getThisMonth `shouldBe` Time.fromGregorian 2000 1 1

  it "gets top day of the next page" $ do
    let db = def
          { DB._afterSavedDays = \day ->
              checkArgument day (Time.fromGregorian 2000 1 2) $
                E.enumList 1
                  [ Time.fromGregorian 2000 1 1
                  , Time.fromGregorian 2000 1 2
                  , Time.fromGregorian 2000 1 3
                  ]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    nav ^! N.getNextPageTop 3 `shouldReturn` Just (Time.fromGregorian 2000 1 3)

  it "gets top day of the previous page" $ do
    let db = def
          { DB._beforeSavedDays = \day ->
              checkArgument day (Time.fromGregorian 1999 12 31) $
                E.enumList 1
                  [ Time.fromGregorian 1999 12 31
                  , Time.fromGregorian 1999 12 30
                  , Time.fromGregorian 1999 12 29
                  ]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 1
    nav ^! N.getPreviousPageTop 3 `shouldReturn` Just (Time.fromGregorian 1999 12 29)

  it "gets the next month" $ do
    let db = def {DB._afterSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 2 1]}
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 15
    nav ^! N.getNextMonth `shouldReturn` Just (Time.fromGregorian 2000 2 1)

  it "gets the previous month" $ do
    let db = def {DB._beforeSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 12 31]}
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 15
    nav ^! N.getPreviousMonth `shouldReturn` Just (Time.fromGregorian 1999 12 1)

  it "gets the no monthes when there are no entries" $ do
    let db = def
          { DB._afterSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 1 5]
          , DB._beforeSavedDays = const $ E.enumList 1 [Time.fromGregorian 2000 1 3]
          }
        nav = N.makeNavigation db $ Time.fromGregorian 2000 1 4
    nav ^! N.getNextMonth `shouldReturn` Nothing
    nav ^! N.getPreviousMonth `shouldReturn` Nothing

checkArgument :: (Show a, Eq a) => a -> a -> b -> b
checkArgument actual expected value
  | expected == actual = value
  | otherwise = error $ "Argument must be: " <> show expected

instance (Functor m, Applicative m, Monad m) => Default (DB.EntryDatabase m) where
  def = DB.EntryDatabase
    { _selectOne = undefined
    , _selectPage = undefined
    , _selectAll = undefined
    , _search = undefined
    , _insert = undefined
    , _update = undefined
    , _delete = undefined
    , _beforeSavedDays = undefined
    , _afterSavedDays = undefined
    , _insertComment = undefined
    }
