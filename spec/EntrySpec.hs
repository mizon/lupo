{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module EntrySpec
  ( entrySpec
  , savedObjectSpec
  ) where

import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Test.Hspec

import qualified Lupo.Backends.Entry as E
import qualified Lupo.Entry as E
import Lupo.Exception
import Lupo.Util

entrySpec :: Spec
entrySpec = describe "database wrapper" $ do
  it "selects an entry" $
    withDB $ \db -> do
      e3 <- E.selectOne db 3
      e3 `shouldSameContent` E.Entry "title 8-16" "body 8-16"

  it "inserts an entry" $ do
    withDB $ \db -> do
      E.insert db $ E.Entry "title newest" "body newest"
      e <- E.selectOne db 6
      e `shouldSameContent` E.Entry "title newest" "body newest"

  it "selects by specified day" $ do
    withDB $ \db -> do
      page <- E.selectPage db $ Time.fromGregorian 2012 8 15
      E.numOfComments page `shouldBe` 0
      let es = E.pageEntries page
      Prelude.length es `shouldBe` 2
      (es !! 0) `shouldSameContent` E.Entry "title 8-15-1" "body 8-15-1"
      (es !! 1) `shouldSameContent` E.Entry "title 8-15-2" "body 8-15-2"

  it "deletes an entry" $ do
    withDB $ \db -> do
      e1 <- E.selectOne db 1
      e1 `shouldSameContent` E.Entry "title 8-15-1" "body 8-15-1"
      E.delete db 1
      E.selectOne db 1 `shouldThrow` \(_ :: RecordNotFound) -> True

  it "updates an entry" $ do
    withDB $ \db -> do
      E.update db 1 $ E.Entry "foo" "foooo"
      e <- E.selectOne db 1
      e `shouldSameContent` E.Entry "foo" "foooo"

  it "selects all entries" $ do
    withDB $ \db -> do
      es <- run_ $ E.selectAll db $$ EL.consume
      Prelude.length es `shouldBe` 5
      (es !! 0) `shouldSameContent` E.Entry "title 8-20-2" "body 8-20-2"
      (es !! 2) `shouldSameContent` E.Entry "title 8-16" "body 8-16"

  it "searches entries" $ do
    withDB $ \db -> do
      es <- run_ $ E.search db "body 8-20" $$ EL.consume
      Prelude.length es `shouldBe` 2
      (es !! 0) `shouldSameContent` E.Entry "title 8-20-2" "body 8-20-2"

  it "selects days which have entries before a specified day" $ do
    withDB $ \db -> do
      days <- run_ $ E.beforeSavedDays db (Time.fromGregorian 2012 8 20) $$ EL.consume
      Prelude.length days `shouldBe` 3
      days !! 0 `shouldSatisfy` (> days !! 2)

  it "selects days which have entries after a specified day" $ do
    withDB $ \db -> do
      days <- run_ $ E.afterSavedDays db (Time.fromGregorian 2012 8 15) $$ EL.consume
      Prelude.length days `shouldBe` 3
      days !! 0 `shouldSatisfy` (< days !! 2)

  it "inserts a comment" $ do
    withDB $ \db -> do
      let new = E.Comment "taro" "hello, there."
      E.insertComment db (Time.fromGregorian 2012 8 16) new
      d <- E.selectPage db $ Time.fromGregorian 2012 8 16
      E.numOfComments d `shouldBe` 1
      let saved = E.savedContent $ Prelude.head $ E.pageComments d
      E.commentName saved `shouldBe` "taro"
      E.commentBody saved `shouldBe` "hello, there."

  it "inserts an empty comment" $ do
    withDB $ \db -> do
      let emptyComment = E.Comment " " " "
      E.insertComment db (Time.fromGregorian 2012 8 16) emptyComment
        `shouldThrow` \(InvalidField msgs) ->
          Prelude.length msgs == 2

  it "denies a spam comments" $ do
    withDB $ \db -> do
      let spamComment = E.Comment "spammer" "foo spam string bar"
      E.insertComment db (Time.fromGregorian 2012 8 16) spamComment
        `shouldThrow` \(InvalidField (msg : _)) ->
          msg == "Comment is invalid."

savedObjectSpec :: Spec
savedObjectSpec = describe "container for persisted object" $
  it "has created day" $ do
    now <- Time.getZonedTime
    E.getCreatedDay (E.Saved 1 now now ()) `shouldBe` zonedDay now
    E.getCreatedDay (E.Saved 1 now (toNextDay now) ()) `shouldBe` zonedDay now
    E.getCreatedDay (E.Saved 1 (toNextDay now) now ()) `shouldSatisfy`(/= zonedDay now)
  where
    toNextDay = zonedTimeToLocalTime %~ localDay %~ Time.addDays 1

shouldSameContent :: E.Saved E.Entry -> E.Entry -> Expectation
shouldSameContent (E.savedContent -> actual) expected = actual `shouldBe` expected

withDB :: (E.EntryDatabase IO -> Expectation) -> Expectation
withDB testBody = bracket initialize finalize $ \conn ->
  testBody =<< E.unEDBWrapper <$> E.makeEntryDatabase conn spamFilter
  where
    initialize = do
      conn <- Sqlite3.connectSqlite3 "./test.sqlite3"
      DB.withTransaction conn $ const $ DB.runRaw conn =<< readFile "./spec/fixture.sql"
      pure conn

    finalize conn = do
      DB.withTransaction conn $ const $ do
        void $ DB.run conn "DELETE FROM entries" []
        void $ DB.run conn "DELETE FROM comments" []
      DB.disconnect conn

    spamFilter E.Comment {..} = not $ T.isInfixOf "spam string" commentBody
