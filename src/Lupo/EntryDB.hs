{-# LANGUAGE OverloadedStrings
    , TemplateHaskell
    , PolymorphicComponents
    , DoAndIfThenElse
    , RecordWildCards
    , ViewPatterns
    , ScopedTypeVariables #-}
module Lupo.EntryDB
    ( MonadEntryDB(..)
    , Entry(..)
    , Saved(..)
    , getCreatedDay
    , EntryDB(..)
    , makeEntryDB
    ) where

import Lupo.Exception
import Lupo.Util
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
    , body :: T.Text
    } deriving (Show, Eq)

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
    , selectDay :: MonadEntryDB m => Ti.Day -> m [Saved Entry]
    , all :: forall m a. MonadEntryDB m => m (Enumerator (Saved Entry) m a)
    , search :: forall m a. MonadEntryDB m => T.Text -> m (Enumerator (Saved Entry) m a)
    , insert :: MonadEntryDB m => Entry -> m ()
    , update :: MonadEntryDB m => Integer -> Entry -> m ()
    , delete :: MonadEntryDB m => Integer -> m ()
    }

getCreatedDay :: Saved a -> Ti.Day
getCreatedDay = Ti.localDay . Ti.zonedTimeToLocalTime . createdAt

makeEntryDB :: DB.IConnection conn => conn -> FilePath -> EntryDB
makeEntryDB conn dir = EntryDB
    { connection = DB.ConnWrapper conn
    , entriesDir = dir
    , select = dbSelect
    , selectDay = dbSelectDay
    , all = dbAll
    , search = dbSearch
    , insert = dbInsert
    , update = dbUpdate
    , delete = dbDelete
    }

dbSelect :: MonadEntryDB m => Integer -> m (Saved Entry)
dbSelect i = do
    conn <- connection <$> getEntryDB
    row <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries WHERE id = ?"
        void $ DB.execute stmt [DB.toSql i]
        DB.fetchRow stmt
    maybe (throw RecordNotFound) fromSql row

dbSelectDay :: MonadEntryDB m => Ti.Day -> m [Saved Entry]
dbSelectDay (DB.toSql -> day) = do
    (connection -> conn) <- getEntryDB
    rows <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries WHERE day_id = ? ORDER BY created_at ASC"
        void $ DB.execute stmt [day]
        DB.fetchAllRows stmt
    Prelude.mapM fromSql rows

dbAll :: MonadEntryDB m => m (Enumerator (Saved Entry) m a)
dbAll = do
    conn <- connection <$> getEntryDB
    rows <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries ORDER BY created_at DESC"
        void $ DB.execute stmt []
        DB.fetchAllRows stmt
    return $ enumList 1 rows $= EL.mapM fromSql

dbSearch :: MonadEntryDB m => T.Text -> m (Enumerator (Saved Entry) m a)
dbSearch = $notImplemented

dbInsert :: MonadEntryDB m => Entry -> m ()
dbInsert Entry {..} = do
    conn <- connection <$> getEntryDB
    liftIO $ do
        now <- Ti.getZonedTime
        void $ DB.run conn "INSERT INTO days (id) SELECT ? WHERE NOT EXISTS (SELECT 1 FROM days WHERE id = ?)"
            [DB.toSql $ zonedDay now, DB.toSql $ zonedDay now]
        void $ DB.run conn "INSERT INTO entries (created_at, modified_at, day_id, title, body) VALUES (?, ?, ?, ?, ?)"
            [ DB.toSql now
            , DB.toSql now
            , DB.toSql $ zonedDay now
            , DB.toSql title
            , DB.toSql body
            ]
        DB.commit conn

dbUpdate :: MonadEntryDB m => Integer -> Entry -> m ()
dbUpdate i Entry {..} = do
    (connection -> conn) <- getEntryDB
    liftIO $ do
        now <- Ti.getZonedTime
        void $ DB.run conn "UPDATE entries SET modified_at = ?, title = ?, body = ? WHERE id = ?"
            [ DB.toSql now
            , DB.toSql title
            , DB.toSql body
            , DB.toSql i
            ]
        DB.commit conn

dbDelete :: MonadEntryDB m => Integer -> m ()
dbDelete i = do
    db <- getEntryDB
    (connection -> conn) <- getEntryDB
    (getCreatedDay -> created) <- select db i
    liftIO $ do
        status <- DB.run conn "DELETE FROM entries WHERE id = ?" [DB.toSql i]
        if status /= 1 then
            throw RecordNotFound
        else do
            void $ DB.run conn "DELETE FROM days WHERE id = ?" [DB.toSql created]
            DB.commit conn

fromSql :: MonadEntryDB m => [DB.SqlValue] -> m (Saved Entry)
fromSql [ DB.fromSql -> id_
        , _
        , DB.fromSql -> c_at
        , DB.fromSql -> m_at
        , DB.fromSql -> t
        , DB.fromSql -> b ] = do
    return Saved
        { idx = id_
        , createdAt = c_at
        , modifiedAt = m_at
        , refObject = Entry {title = t, body = b}
        }
fromSql _ = undefined
