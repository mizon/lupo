{-# OPTIONS_GHC -fno-warn-orphans #-}

module URLMapperSpec
  ( urlMapperSpec
  ) where

import qualified Data.ByteString as BS
import qualified Data.Time as Time
import Test.Hspec
import Test.QuickCheck

import qualified Lupo.Backends.URLMapper as U
import qualified Lupo.Entry as E
import Lupo.Import
import qualified Lupo.URLMapper as U

urlMapperSpec :: Spec
urlMapperSpec = describe "URL Mapper" $ do
  it "provides a path to the single entry page" $
    U.entryPath (E.Saved 8 undefined undefined undefined) `shouldGet`
      "/lupo/entries/8"

  it "provides a path to the entry editor page" $
    U.entryEditPath (E.Saved 8 undefined undefined undefined) `shouldGet`
      "/lupo/admin/8/edit"

  it "provides a path to the day page" $
    U.singleDayPath (Time.fromGregorian 2012 1 1) `shouldGet`
      "/lupo/20120101"

  it "provides a path to the month page" $
    U.monthPath (Time.fromGregorian 2012 1 1) `shouldGet`
      "/lupo/201201"

  it "provides a path to multi entries" $
    U.multiDaysPath (Time.fromGregorian 2012 1 1) 5 `shouldGet`
      "/lupo/20120101-5"

  it "provides a path to the top page" $
    U.topPagePath `shouldGet` "/lupo/"

  it "provides a path to the admin page" $
    U.adminPath `shouldGet` "/lupo/admin"

  it "provides a path to the initAccount page" $
    U.initAccountPath `shouldGet` "/lupo/init-account"

  it "provides a path to posting comment" $ do
    U.commentPostPath (Time.fromGregorian 2012 1 1) `shouldGet`
      "/lupo/20120101/comment#new-comment"
    U.newCommentPath (Time.fromGregorian 2012 1 1) `shouldGet`
      "/lupo/20120101#new-comment"
    U.commentsPath (Time.fromGregorian 2012 1 1) `shouldGet`
      "/lupo/20120101#comments"

  it "provides a path to css files" $
    U.cssPath "celeste.css" `shouldGet` "/lupo/css/celeste.css"

  it "provides full pathes whatever you like" $
    property $ \path ->
      urlMapper ^. U.fullPath path == "/lupo/" <> path
  where
    shouldGet getter expected = urlMapper ^. getter `shouldBe` expected
    urlMapper = U.makeURLMapper "/lupo"

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
