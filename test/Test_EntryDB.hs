{-# LANGUAGE
      OverloadedStrings
    , ScopedTypeVariables
    , FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , TypeFamilies #-}
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
import System.IO
import Control.Monad.CatchIO
import Control.Monad.Reader
import Control.Applicative
import qualified Control.Exception as E
import Prelude hiding (catch)

instance (MonadReader EDB.EntryDB m, MonadCatchIO m, Applicative m, Functor m) =>
        EDB.MonadEntryDB m where
    getEntryDB = ask

dbTest :: Test
dbTest = testGroup "database control"
    [ dbTestCase "select" $ do
        db <- EDB.getEntryDB
        e3 <- EDB.select db 3
        assertEntry (EDB.Entry "title 8-16" "body 8-16") e3

    , dbTestCase "insert" $ do
        db <- EDB.getEntryDB
        EDB.insert db $ EDB.Entry "title newest" "body newest"
        e <- EDB.select db 6
        assertEntry (EDB.Entry "title newest" "body newest") e

    , dbTestCase "select by day" $ do
        db <- EDB.getEntryDB
        es <- EDB.selectDay db $ Ti.fromGregorian 2012 8 15
        liftIO $ Prelude.length es @?= 2
        assertEntry (EDB.Entry "title 8-15-1" "body 8-15-1") $ es !! 0
        assertEntry (EDB.Entry "title 8-15-2" "body 8-15-2") $ es !! 1

    , dbTestCase "delete" $ do
        db <- EDB.getEntryDB
        e1 <- EDB.select db 1
        assertEntry (EDB.Entry "title 8-15-1" "body 8-15-1") e1
        EDB.delete db 1
        assertRaise RecordNotFound $
            void $ EDB.select db 1

    , dbTestCase "update" $ do
        db <- EDB.getEntryDB
        EDB.update db 1 $ EDB.Entry "foo" "foooo"
        e <- EDB.select db 1
        assertEntry (EDB.Entry "foo" "foooo") e

    , dbTestCase "all" $ do
        db <- EDB.getEntryDB
        enum <- EDB.all db
        es <- run_ $ enum $$ EL.consume
        liftIO $ Prelude.length es @?= 5
        assertEntry (EDB.Entry "title 8-20-2" "body 8-20-2") $ es !! 0
        assertEntry (EDB.Entry "title 8-16" "body 8-16") $ es !! 2

    , dbTestCase "search" $ do
        db <- EDB.getEntryDB
        es <- run_ =<< ($$ EL.consume) <$> EDB.search db "body 8-20"
        liftIO $ Prelude.length es @?= 2
        assertEntry (EDB.Entry "title 8-20-2" "body 8-20-2") $ es !! 0

    , dbTestCase "days before" $ do
        db <- EDB.getEntryDB
        days <- run_ =<< ($$ EL.consume) <$> EDB.beforeSavedDays db (Ti.fromGregorian 2012 8 20)
        liftIO $ do
            Prelude.length days @?= 3
            days !! 0 > days !! 2 @? "must desc"

    , dbTestCase "days after" $ do
        db <- EDB.getEntryDB
        days <- run_ =<< ($$ EL.consume) <$> EDB.afterSavedDays db (Ti.fromGregorian 2012 8 15)
        liftIO $ do
            Prelude.length days @?= 3
            days !! 0 < days !! 2 @? "must asc"
    ]

savedTest :: Test
savedTest = testGroup "saved object"
    [ testCase "getCreatedDay" $ do
        now <- Ti.getZonedTime
        EDB.getCreatedDay (EDB.Saved 1 now now ()) @?= getDay now
        EDB.getCreatedDay (EDB.Saved 1 now (toNextDay now) ()) @?= getDay now
        EDB.getCreatedDay (EDB.Saved 1 (toNextDay now) now ()) /= getDay now @?
            "must not true when not equal createdAt"
    ]
  where
    toNextDay d = d {Ti.zonedTimeToLocalTime = (Ti.zonedTimeToLocalTime d) {Ti.localDay = getNextDay}}
      where
        getNextDay = Ti.addDays 1 $ getDay d

    getDay = Ti.localDay . Ti.zonedTimeToLocalTime

assertEntry :: MonadIO m => EDB.Entry -> EDB.Saved EDB.Entry -> m ()
assertEntry expected actual
    | expected == EDB.refObject actual = return ()
    | otherwise = liftIO $ assertFailure "invlaid entry"

dbTestCase :: String -> ReaderT EDB.EntryDB IO () -> Test
dbTestCase msg m = testCase msg $
    bracket initialize finalize $ \conn ->
        runReaderT m $ EDB.makeEntryDB conn
  where
    initialize = do
        conn <- Sqlite3.connectSqlite3 "./test.sqlite3"
        DB.runRaw conn =<< readFile "./test/fixture.sql"
        DB.commit conn
        return conn

    finalize conn = do
        void $ DB.run conn "DELETE FROM entries" []
        DB.commit conn
        DB.disconnect conn

assertRaise :: (MonadCatchIO m, E.Exception e) => e -> m () -> m ()
assertRaise ex m = (m >> liftIO (assertFailure "exception doesn't be raised")) `catch`
    \(raised :: LupoException) -> do
        unless (show ex == show raised) $
            liftIO $ assertFailure "an exception raised but it isn't expected"
