{-# LANGUAGE OverloadedStrings
    , ScopedTypeVariables
    , FlexibleInstances #-}
module Test_EntryDB
    ( dbTest
    , savedTest
    ) where

import qualified Lupo.EntryDB as EDB
import Lupo.Exception
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (Test)
import qualified Data.Enumerator.List as EL
import Data.Enumerator
import qualified Data.Time as Ti
import Control.Monad.CatchIO
import Control.Monad.Reader
import Control.Applicative
import qualified Control.Exception as E
import qualified System.Directory as D
import Prelude hiding (catch)

instance (MonadCatchIO m, Applicative m, Functor m) =>
        EDB.MonadEntryDB (ReaderT EDB.EntryDB m) where
    getEntryDB = ask

dbTest :: Test
dbTest = testGroup "database control"
    [ dbTestCase "insert and select" $ do
        db <- EDB.getEntryDB
        EDB.insert db $ EDB.Entry "title" "body"
        e <- EDB.select db 1
        assertEntry (EDB.Entry "title" "body") e

    , dbTestCase "delete" $ do
        db <- EDB.getEntryDB
        EDB.insert db $ EDB.Entry "title" "body"
        EDB.delete db 1
        assertRaise RecordNotFound $
            void $ EDB.select db 1

    , dbTestCase "update" $ do
        db <- EDB.getEntryDB
        EDB.insert db $ EDB.Entry "title" "body"
        EDB.update db 1 $ EDB.Entry "foo" "foooo"
        e <- EDB.select db 1
        assertEntry (EDB.Entry "foo" "foooo") e

    , dbTestCase "all empty" $ do
        db <- EDB.getEntryDB
        enum <- EDB.all db
        es <- run_ $ enum $$ EL.consume
        liftIO $ Prelude.length es @?= 0

    , dbTestCase "all exist" $ do
        db <- EDB.getEntryDB
        EDB.insert db $ EDB.Entry "foo1" "body"
        EDB.insert db $ EDB.Entry "foo2" "body"
        enum <- EDB.all db
        es <- run_ $ enum $$ EL.consume
        liftIO $ Prelude.length es @?= 2
        assertEntry (EDB.Entry "foo2" "body") $ es !! 0
        assertEntry (EDB.Entry "foo1" "body") $ es !! 1
    ]

savedTest :: Test
savedTest = testGroup "saved object"
    [ testCase "compare same createdAt" $ do
        now <- Ti.getZonedTime
        EDB.isSameCreatedDay (EDB.Saved 0 now now ()) (EDB.Saved 1 now now ()) @? "must true"

    , testCase "compare not same createdAt" $ do
        now <- Ti.getZonedTime
        not (EDB.isSameCreatedDay (EDB.Saved 0 (toNextDay now) now ()) (EDB.Saved 1 now now ())) @? "must false"

    , testCase "compare not same modifiedAt" $ do
        now <- Ti.getZonedTime
        EDB.isSameCreatedDay (EDB.Saved 0 now (toNextDay now) ()) (EDB.Saved 1 now now ()) @? "must true"
    ]
  where
    toNextDay d = d {Ti.zonedTimeToLocalTime = (Ti.zonedTimeToLocalTime d) {Ti.localDay = getNextDay}}
      where
        getNextDay = Ti.addDays 1 $ Ti.localDay $ Ti.zonedTimeToLocalTime d

assertEntry :: MonadIO m => EDB.Entry -> EDB.Saved EDB.Entry -> m ()
assertEntry expected actual
    | expected == EDB.refObject actual = return ()
    | otherwise = liftIO $ assertFailure "invlaid entry"

dbTestCase :: String -> ReaderT EDB.EntryDB IO () -> Test
dbTestCase msg m = testCase msg $
    bracket initialize finalize $ \conn ->
        runReaderT m $ EDB.makeEntryDB conn entriesPath
  where
    initialize = do
        D.createDirectory entriesPath
        Sqlite3.connectSqlite3 "./test.sqlite3"

    finalize conn = do
        D.removeDirectoryRecursive entriesPath
        void $ DB.run conn "DELETE FROM entries" []
        DB.commit conn
        DB.disconnect conn

    entriesPath = "./tmp/entries"

assertRaise :: (MonadCatchIO m, E.Exception e) => e -> m () -> m ()
assertRaise ex m = (m >> liftIO (assertFailure "exception doesn't be raised")) `catch`
    \(raised :: LupoException) -> do
        unless (show ex == show raised) $
            liftIO $ assertFailure "an exception raised but it isn't expected"
