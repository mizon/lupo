{-# LANGUAGE TemplateHaskell
    , FlexibleInstances
    , OverloadedStrings
    , ScopedTypeVariables #-}
import qualified Lupo.EntryDB as EDB
import Lupo.Exception
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (Test)
import qualified Data.Enumerator.List as EL
import Data.Enumerator
import Control.Monad.CatchIO
import Control.Monad.Reader
import Control.Applicative
import qualified Control.Exception as E
import Prelude hiding (catch)

main :: IO ()
main = defaultMain testSuite

instance (MonadCatchIO m, Applicative m, Functor m) =>
        EDB.MonadEntryDB (ReaderT EDB.EntryDB m) where
    getEntryDB = ask

testSuite :: [Test]
testSuite =
    [ testGroup "database control"
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
    ]

assertEntry :: MonadIO m => EDB.Entry -> EDB.Saved EDB.Entry -> m ()
assertEntry expected actual
    | expected == EDB.refObject actual = return ()
    | otherwise = liftIO $ assertFailure "invlaid entry"

dbTestCase :: String -> ReaderT EDB.EntryDB IO () -> Test
dbTestCase msg m = testCase msg $
    bracket (Sqlite3.connectSqlite3 "./test.sqlite3") finalize $ \conn ->
        runReaderT m $ EDB.makeEntryDB conn "./tmp/entries"
  where
    finalize conn = do
        void $ DB.run conn "DELETE FROM entries" []
        DB.commit conn
        DB.disconnect conn

assertRaise :: (MonadCatchIO m, E.Exception e) => e -> m () -> m ()
assertRaise ex m = (m >> liftIO (assertFailure "exception doesn't be raised")) `catch`
    \(raised :: LupoException) -> do
        unless (show ex == show raised) $
            liftIO $ assertFailure "an exception raised but it isn't expected"
