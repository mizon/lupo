{-# LANGUAGE OverloadedStrings #-}
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
      e3 <- db ^! E.selectOne 3
      e3 `shouldSameContent` E.Entry "title 8-16" "body 8-16"

  it "inserts an entry" $ do
    withDB $ \db -> do
      db ^! E.insert (E.Entry "title newest" "body newest")
      e <- db ^! E.selectOne 6
      e `shouldSameContent` E.Entry "title newest" "body newest"

  it "selects by specified day" $ do
    withDB $ \db -> do
      page <- db ^! E.selectPage (Time.fromGregorian 2012 8 15)
      page ^. E.numOfComments `shouldBe` 0
      let es = page ^. E.pageEntries
      Prelude.length es `shouldBe` 2
      (es !! 0) `shouldSameContent` E.Entry "title 8-15-1" "body 8-15-1"
      (es !! 1) `shouldSameContent` E.Entry "title 8-15-2" "body 8-15-2"

  it "deletes an entry" $ do
    withDB $ \db -> do
      e1 <- db ^! E.selectOne 1
      e1 `shouldSameContent` E.Entry "title 8-15-1" "body 8-15-1"
      db ^! E.delete 1
      db ^! E.selectOne 1 `shouldThrow` \(_ :: RecordNotFound) -> True

  it "updates an entry" $ do
    withDB $ \db -> do
      db ^! E.update 1 (E.Entry "foo" "foooo")
      e <- db ^! E.selectOne 1
      e `shouldSameContent` E.Entry "foo" "foooo"

  it "selects all entries" $ do
    withDB $ \db -> do
      es <- run_ $ db ^. E.selectAll $$ EL.consume
      Prelude.length es `shouldBe` 5
      (es !! 0) `shouldSameContent` E.Entry "title 8-20-2" "body 8-20-2"
      (es !! 2) `shouldSameContent` E.Entry "title 8-16" "body 8-16"

  it "searches entries" $ do
    withDB $ \db -> do
      es <- run_ $ db ^. E.search "body 8-20" $$ EL.consume
      Prelude.length es `shouldBe` 2
      (es !! 0) `shouldSameContent` E.Entry "title 8-20-2" "body 8-20-2"

  it "selects days which have entries before a specified day" $ do
    withDB $ \db -> do
      days <- run_ $ db ^. E.beforeSavedDays (Time.fromGregorian 2012 8 20) $$ EL.consume
      Prelude.length days `shouldBe` 3
      days !! 0 `shouldSatisfy` (> days !! 2)

  it "selects days which have entries after a specified day" $ do
    withDB $ \db -> do
      days <- run_ $ db ^. E.afterSavedDays (Time.fromGregorian 2012 8 15) $$ EL.consume
      Prelude.length days `shouldBe` 3
      days !! 0 `shouldSatisfy` (< days !! 2)

  it "inserts a comment" $ do
    withDB $ \db -> do
      let new = E.Comment "taro" "hello, there."
      db ^! E.insertComment (Time.fromGregorian 2012 8 16) new
      d <- db ^! E.selectPage (Time.fromGregorian 2012 8 16)
      d ^. E.numOfComments `shouldBe` 1
      let saved = d ^. E.pageComments . to Prelude.head . E.savedContent
      saved ^. E.commentName `shouldBe` "taro"
      saved ^. E.commentBody `shouldBe` "hello, there."

  it "inserts an empty comment" $ do
    withDB $ \db -> do
      let emptyComment = E.Comment " " " "
      db ^! E.insertComment (Time.fromGregorian 2012 8 16) emptyComment
        `shouldThrow` \(InvalidField msgs) ->
          Prelude.length msgs == 2

  it "denies a spam comments" $ do
    withDB $ \db -> do
      let spamComment = E.Comment "spammer" "foo spam string bar"
      db ^! E.insertComment (Time.fromGregorian 2012 8 16) spamComment
        `shouldThrow` \(InvalidField (msg : _)) ->
          msg == "Comment is invalid."

savedObjectSpec :: Spec
savedObjectSpec = describe "container for persisted object" $
  it "has created day" $ do
    now <- Time.getZonedTime
    E.Saved 1 now now () ^. E.getCreatedDay `shouldBe` zonedDay now
    E.Saved 1 now (toNextDay now) () ^. E.getCreatedDay `shouldBe` zonedDay now
    E.Saved 1 (toNextDay now) now () ^. E.getCreatedDay `shouldSatisfy` (/= zonedDay now)
  where
    toNextDay = zonedTimeToLocalTime %~ localDay %~ Time.addDays 1

shouldSameContent :: E.Saved E.Entry -> E.Entry -> Expectation
shouldSameContent (view E.savedContent -> actual) expected = actual `shouldBe` expected

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

    spamFilter c = not $ T.isInfixOf "spam string" $ c ^. E.commentBody
