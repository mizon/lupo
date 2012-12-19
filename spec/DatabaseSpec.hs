{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module DatabaseSpec
  ( databaseSpec
  , savedObjectSpec
  ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Reader
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import Data.Lens.Common
import qualified Data.Time as Time
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Prelude hiding (catch)
import Test.Hspec

import qualified Lupo.Database as LDB
import Lupo.Exception
import Lupo.Util

databaseSpec :: Spec
databaseSpec = describe "database wrapper" $ do
  it "selects an entry" $
    withDB $ \db -> do
      e3 <- LDB.select db 3
      e3 `shouldSameContent` LDB.Entry "title 8-16" "body 8-16"

  it "inserts an entry" $ do
    withDB $ \db -> do
      LDB.insert db $ LDB.Entry "title newest" "body newest"
      e <- LDB.select db 6
      e `shouldSameContent` LDB.Entry "title newest" "body newest"

  it "selects by specified day" $ do
    withDB $ \db -> do
      d <- LDB.selectDay db $ Time.fromGregorian 2012 8 15
      LDB.numOfComments d `shouldBe` 0
      let es = LDB.dayEntries d
      Prelude.length es `shouldBe` 2
      (es !! 0) `shouldSameContent` LDB.Entry "title 8-15-1" "body 8-15-1"
      (es !! 1) `shouldSameContent` LDB.Entry "title 8-15-2" "body 8-15-2"

  it "deletes an entry" $ do
    withDB $ \db -> do
      e1 <- LDB.select db 1
      e1 `shouldSameContent` LDB.Entry "title 8-15-1" "body 8-15-1"
      LDB.delete db 1
      LDB.select db 1 `shouldThrow` \(_ :: RecordNotFound) -> True

  it "updates an entry" $ do
    withDB $ \db -> do
      LDB.update db 1 $ LDB.Entry "foo" "foooo"
      e <- LDB.select db 1
      e `shouldSameContent` LDB.Entry "foo" "foooo"

  it "selects all entries" $ do
    withDB $ \db -> do
      enum <- LDB.all db
      es <- run_ $ enum $$ EL.consume
      Prelude.length es `shouldBe` 5
      (es !! 0) `shouldSameContent` LDB.Entry "title 8-20-2" "body 8-20-2"
      (es !! 2) `shouldSameContent` LDB.Entry "title 8-16" "body 8-16"

  it "searches entries" $ do
    withDB $ \db -> do
      es <- run_ =<< ($$ EL.consume) <$> LDB.search db "body 8-20"
      Prelude.length es `shouldBe` 2
      (es !! 0) `shouldSameContent` LDB.Entry "title 8-20-2" "body 8-20-2"

  it "selects days which have entries before a specified day" $ do
    withDB $ \db -> do
      days <- run_ =<< ($$ EL.consume) <$>
        LDB.beforeSavedDays db (Time.fromGregorian 2012 8 20)
      Prelude.length days `shouldBe` 3
      days !! 0 `shouldSatisfy` (> days !! 2)

  it "selects days which have entries after a specified day" $ do
    withDB $ \db -> do
      days <- run_ =<< ($$ EL.consume) <$>
        LDB.afterSavedDays db (Time.fromGregorian 2012 8 15)
      Prelude.length days `shouldBe` 3
      days !! 0 `shouldSatisfy` (< days !! 2)

  it "inserts a comment" $ do
    withDB $ \db -> do
      let new = LDB.Comment "taro" "hello, there."
      LDB.insertComment db (Time.fromGregorian 2012 8 16) new
      d <- LDB.selectDay db $ Time.fromGregorian 2012 8 16
      LDB.numOfComments d `shouldBe` 1
      let saved = LDB.savedContent $ Prelude.head $ LDB.dayComments d
      LDB.commentName saved `shouldBe` "taro"
      LDB.commentBody saved `shouldBe` "hello, there."

  it "inserts an empty comment" $ do
    withDB $ \db -> do
      let emptyComment = LDB.Comment " " " "
      LDB.insertComment db (Time.fromGregorian 2012 8 16) emptyComment
        `shouldThrow` \(InvalidField msgs) ->
          Prelude.length msgs == 2

savedObjectSpec :: Spec
savedObjectSpec = describe "container for persisted object" $
  it "has created day" $ do
    now <- Time.getZonedTime
    LDB.getCreatedDay (LDB.Saved 1 now now ()) `shouldBe` zonedDay now
    LDB.getCreatedDay (LDB.Saved 1 now (toNextDay now) ()) `shouldBe` zonedDay now
    LDB.getCreatedDay (LDB.Saved 1 (toNextDay now) now ()) `shouldSatisfy`(/= zonedDay now)
  where
    toNextDay = zonedTimeToLocalTime ^%= localDay ^%= Time.addDays 1

shouldSameContent :: LDB.Saved LDB.Entry -> LDB.Entry -> Expectation
shouldSameContent (LDB.savedContent -> actual) expected = actual `shouldBe` expected

withDB :: (LDB.Database IO -> Expectation) -> Expectation
withDB testBody = bracket initialize finalize $ \conn ->
  testBody $ LDB.makeDatabase conn
  where
    initialize = do
      conn <- Sqlite3.connectSqlite3 "./test.sqlite3"
      DB.runRaw conn =<< readFile "./spec/fixture.sql"
      DB.commit conn
      pure conn

    finalize conn = do
      void $ DB.run conn "DELETE FROM entries" []
      void $ DB.run conn "DELETE FROM comments" []
      DB.commit conn
      DB.disconnect conn
