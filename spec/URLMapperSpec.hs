{-# LANGUAGE OverloadedStrings #-}
module URLMapperSpec (
    urlMapperSpec
  ) where

import qualified Data.Time as Time
import Test.Hspec

import qualified Lupo.Database as LDB
import qualified Lupo.URLMapper as U

urlMapperSpec :: Spec
urlMapperSpec = describe "URL Mapper" $ do
  it "provides a path to the single entry page" $
    U.entryPath urlMapper (LDB.Saved 8 undefined undefined undefined) `shouldBe`
      "/lupo/entries/8"

  it "provides a path to the entry editor page" $
    U.entryEditPath urlMapper (LDB.Saved 8 undefined undefined undefined) `shouldBe`
      "/lupo/admin/8/edit"

  it "provides a path to the day page" $
    U.singleDayPath urlMapper (Time.fromGregorian 2012 1 1) `shouldBe`
      "/lupo/20120101"

  it "provides a path to the month page" $
    U.monthPath urlMapper (Time.fromGregorian 2012 1 1) `shouldBe`
      "/lupo/201201"

  it "provides a path to multi entries" $
    U.multiDaysPath urlMapper (Time.fromGregorian 2012 1 1) 5 `shouldBe`
      "/lupo/20120101-5"

  it "provides a path to the top page" $
    U.topPagePath urlMapper `shouldBe` "/lupo/"

  it "provides a path to the admin page" $
    U.adminPath urlMapper `shouldBe` "/lupo/admin"

  it "provides a path to the initAccount page" $
    U.initAccountPath urlMapper `shouldBe` "/lupo/init-account"

  it "provides a path to posting comment" $
    U.commentPath urlMapper (Time.fromGregorian 2012 1 1) `shouldBe`
      "/lupo/20120101/comment"
  where
    urlMapper = U.makeURLMapper "/lupo"
