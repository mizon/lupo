{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lupo.Test.Database
  ( dbTest
  , savedTest
  ) where

import Control.Applicative
import qualified Control.Exception as E
import Control.Monad.CatchIO
import Control.Monad.Reader
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Ti
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)
import Prelude hiding (catch)

import qualified Lupo.Database as LDB
import Lupo.Exception

dbTest :: Test
dbTest = testGroup "database control"
  [ testCase "select" $
      withDB $ \db -> do
        e3 <- LDB.select db 3
        assertEntry (LDB.Entry "title 8-16" "body 8-16") e3

  , testCase "insert" $
      withDB $ \db -> do
        LDB.insert db $ LDB.Entry "title newest" "body newest"
        e <- LDB.select db 6
        assertEntry (LDB.Entry "title newest" "body newest") e

  , testCase "select by day" $
      withDB $ \db -> do
        es <- LDB.selectDay db $ Ti.fromGregorian 2012 8 15
        Prelude.length es @?= 2
        assertEntry (LDB.Entry "title 8-15-1" "body 8-15-1") $ es !! 0
        assertEntry (LDB.Entry "title 8-15-2" "body 8-15-2") $ es !! 1

  , testCase "delete" $
      withDB $ \db -> do
        e1 <- LDB.select db 1
        assertEntry (LDB.Entry "title 8-15-1" "body 8-15-1") e1
        LDB.delete db 1
        assertRaise RecordNotFound $
          void $ LDB.select db 1

  , testCase "update" $
      withDB $ \db -> do
        LDB.update db 1 $ LDB.Entry "foo" "foooo"
        e <- LDB.select db 1
        assertEntry (LDB.Entry "foo" "foooo") e

  , testCase "all" $
      withDB $ \db -> do
        enum <- LDB.all db
        es <- run_ $ enum $$ EL.consume
        Prelude.length es @?= 5
        assertEntry (LDB.Entry "title 8-20-2" "body 8-20-2") $ es !! 0
        assertEntry (LDB.Entry "title 8-16" "body 8-16") $ es !! 2

  , testCase "search" $
      withDB $ \db -> do
        es <- run_ =<< ($$ EL.consume) <$> LDB.search db "body 8-20"
        Prelude.length es @?= 2
        assertEntry (LDB.Entry "title 8-20-2" "body 8-20-2") $ es !! 0

  , testCase "days before" $
      withDB $ \db -> do
        days <- run_ =<< ($$ EL.consume) <$> LDB.beforeSavedDays db (Ti.fromGregorian 2012 8 20)
        Prelude.length days @?= 3
        days !! 0 > days !! 2 @? "must desc"

  , testCase "days after" $
      withDB $ \db -> do
        days <- run_ =<< ($$ EL.consume) <$> LDB.afterSavedDays db (Ti.fromGregorian 2012 8 15)
        Prelude.length days @?= 3
        days !! 0 < days !! 2 @? "must asc"
  ]

savedTest :: Test
savedTest = testGroup "saved object"
  [ testCase "getCreatedDay" $ do
      now <- Ti.getZonedTime
      LDB.getCreatedDay (LDB.Saved 1 now now ()) @?= getDay now
      LDB.getCreatedDay (LDB.Saved 1 now (toNextDay now) ()) @?= getDay now
      LDB.getCreatedDay (LDB.Saved 1 (toNextDay now) now ()) /= getDay now @?
        "must not true when not equal createdAt"
  ]
  where
    toNextDay d = d
      { Ti.zonedTimeToLocalTime = (Ti.zonedTimeToLocalTime d) {Ti.localDay = getNextDay}
      }
      where
        getNextDay = Ti.addDays 1 $ getDay d

    getDay = Ti.localDay . Ti.zonedTimeToLocalTime

assertEntry :: MonadIO m => LDB.Entry -> LDB.Saved LDB.Entry -> m ()
assertEntry expected actual
  | expected == LDB.refObject actual = return ()
  | otherwise = liftIO $ assertFailure "invlaid entry"

withDB :: (LDB.Database IO -> Assertion) -> Assertion
withDB testBody = bracket initialize finalize $ \conn ->
  testBody $ LDB.makeDatabase conn
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
