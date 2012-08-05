{-# LANGUAGE TemplateHaskell
    , FlexibleInstances
    , OverloadedStrings #-}
import qualified Lupo.EntryDB as EDB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import qualified Data.Text as T
import Test.Framework.Providers.HUnit
import Test.Framework
import Test.HUnit hiding (Test)
import Control.Monad.Reader
import Control.Applicative
import Control.Exception

main :: IO ()
main = defaultMain testSuite

instance (MonadIO m, Applicative m, Functor m) =>
        EDB.MonadEntryDB (ReaderT EDB.EntryDB m) where
    getEntryDB = ask

testSuite :: [Test]
testSuite =
    [ testGroup "database control"
        [ dbTestCase "insert" $ do
            db <- EDB.getEntryDB
            EDB.insert db EDB.Entry {EDB.title = "diary title"}
            e <- EDB.select db 1
            assertEntry e 1 "diary title"

        , dbTestCase "delete" $ do
            db <- EDB.getEntryDB
            EDB.insert db EDB.Entry {EDB.title = "foo"}
            EDB.delete db 1
            e <- EDB.select db 1
            assertEntry e 1 "diary title"
        ]
    ]

assertEntry :: MonadIO m => EDB.Saved EDB.Entry -> Integer -> T.Text -> m ()
assertEntry e i title
    | EDB.title (EDB.refObject e) == title && EDB.idx e == i = return ()
    | otherwise = liftIO $ assertFailure "invlaid entry"

dbTestCase :: String -> ReaderT EDB.EntryDB IO () -> Test
dbTestCase msg m = testCase msg $
    bracket (Sqlite3.connectSqlite3 "./test.sqlite3") finalize $ \conn ->
        runReaderT m $ EDB.makeEntryDB conn ""
  where
    finalize conn = do
        void $ DB.run conn "DELETE FROM entries" []
        DB.commit conn
        DB.disconnect conn
