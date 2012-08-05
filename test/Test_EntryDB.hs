{-# LANGUAGE FlexibleInstances
    , FlexibleContexts
    , UndecidableInstances
    , GeneralizedNewtypeDeriving
    , OverloadedStrings #-}
module Test_EntryDB where

import qualified Lupo.EntryDB as EDB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as HDB
import qualified Data.Text as T
import Test.HUnit
import Control.Monad.Reader
import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Functor

instance EDB.MonadEntryDB (ReaderT EDB.EntryDB IO) where
    getEntryDB = ask

_test :: Test
_test = TestCase $ do
    edb <- EDB.makeEntryDB <$> (HDB.ConnWrapper <$> Sqlite3.connectSqlite3 "./test.sqlite3") <*> pure ""
    flip runReaderT edb $ do
        e <- EDB.select edb 1
        liftIO $ assertEntry e 1 "fooo"

assertEntry :: EDB.Saved EDB.Entry -> Integer -> T.Text -> Assertion
assertEntry e i title
    | EDB.title (EDB.refObject e) == title && EDB.idx e == i = return ()
    | otherwise = assertFailure "invlaid entry"
