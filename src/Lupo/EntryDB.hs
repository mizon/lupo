{-# LANGUAGE OverloadedStrings
    , TemplateHaskell
    , PolymorphicComponents
    , DoAndIfThenElse #-}
module Lupo.EntryDB
    ( MonadEntryDB(..)
    , Entry(..)
    , Saved(..)
    , EntryDB(..)
    , makeEntryDB
    ) where

import Lupo.Exception
import qualified Database.HDBC as DB
import qualified Data.Enumerator.List as EL
import Data.Enumerator
import qualified Data.Time as Ti
import qualified Data.Text as T
import Control.Applicative
import Data.Functor
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO
import System.FilePath
import Prelude hiding (all)

import Development.Placeholders

class (MonadCatchIO m, Applicative m, Functor m) => MonadEntryDB m where
    getEntryDB :: m EntryDB

data Entry = Entry
    { title :: T.Text
    } deriving Show

data Saved o = Saved
    { idx :: Integer
    , createdAt :: Ti.ZonedTime
    , modifiedAt :: Ti.ZonedTime
    , refObject :: o
    } deriving Show

data EntryDB = EntryDB
    { connection :: DB.ConnWrapper
    , entriesDir :: FilePath
    , select :: MonadEntryDB m => Integer -> m (Saved Entry)
    , all :: forall m a. MonadEntryDB m => m (Enumerator (Saved Entry) m a)
    , search :: forall m a. MonadEntryDB m => T.Text -> m (Enumerator (Saved Entry) m a)
    , insert :: MonadEntryDB m => Entry -> m ()
    , delete :: MonadEntryDB m => Integer -> m ()
    }

makeEntryDB :: DB.IConnection conn => conn -> FilePath -> EntryDB
makeEntryDB conn dir = EntryDB
    { connection = DB.ConnWrapper conn
    , entriesDir = dir
    , select = dbSelect
    , all = dbAll
    , search = dbSearch
    , insert = dbInsert
    , delete = dbDelete
    }

dbSelect :: MonadEntryDB m => Integer -> m (Saved Entry)
dbSelect i = do
    conn <- connection <$> getEntryDB
    row <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries WHERE id = ?"
        void $ DB.execute stmt [DB.toSql i]
        DB.fetchRow stmt
    maybe (throw RecordNotFound) (return . fromSql) row

dbAll :: MonadEntryDB m => m (Enumerator (Saved Entry) m a)
dbAll = do
    conn <- connection <$> getEntryDB
    rows <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries ORDER BY created_at DESC"
        void $ DB.execute stmt []
        DB.fetchAllRows stmt
    return $ enumList 128 rows $= EL.map fromSql

dbSearch :: MonadEntryDB m => T.Text -> m (Enumerator (Saved Entry) m a)
dbSearch = $notImplemented

dbInsert :: MonadEntryDB m => Entry -> m ()
dbInsert e = do
    conn <- connection <$> getEntryDB
    liftIO $ do
        now <- Ti.getZonedTime
        void $ DB.run conn "INSERT INTO entries (created_at, modified_at, title) VALUES (?, ?, ?)"
            [ DB.toSql now
            , DB.toSql now
            , DB.toSql $ title e
            ]
        DB.commit conn

dbDelete :: MonadEntryDB m => Integer -> m ()
dbDelete i = do
    conn <- connection <$> getEntryDB
    liftIO $ do
        status <- DB.run conn "DELETE FROM entries WHERE id = ?" [DB.toSql i]
        if status /= 1 then
            throw RecordNotFound
        else
            DB.commit conn

fromSql :: [DB.SqlValue] -> Saved Entry
fromSql [id_, c_at, m_at, t] = Saved
    { idx = DB.fromSql id_
    , createdAt = DB.fromSql c_at
    , modifiedAt = DB.fromSql m_at
    , refObject = Entry {title = DB.fromSql t}
    }
fromSql _ = undefined
