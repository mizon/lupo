{-# LANGUAGE OverloadedStrings
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
import Data.Monoid
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO
import Prelude hiding (all)

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
    { select :: MonadEntryDB m => Integer -> m (Saved Entry)
    , selectDay :: MonadEntryDB m => Ti.Day -> m [Saved Entry]
    , all :: MonadEntryDB m => m (Enumerator (Saved Entry) m a)
    , search :: MonadEntryDB m => T.Text -> m (Enumerator (Saved Entry) m a)
    , insert :: MonadEntryDB m => Entry -> m ()
    , update :: MonadEntryDB m => Integer -> Entry -> m ()
    , delete :: MonadEntryDB m => Integer -> m ()
    , beforeSavedDays :: MonadEntryDB m => Ti.Day -> m (Enumerator Ti.Day m a)
    , afterSavedDays :: MonadEntryDB m => Ti.Day -> m (Enumerator Ti.Day m a)
    }

getCreatedDay :: Saved a -> Ti.Day
getCreatedDay = zonedDay . createdAt

makeEntryDB :: DB.IConnection conn => conn -> EntryDB
makeEntryDB conn = EntryDB
    { select = dbSelect
    , selectDay = dbSelectDay
    , all = dbAll
    , search = dbSearch
    , insert = dbInsert
    , update = dbUpdate
    , delete = dbDelete
    , beforeSavedDays = dbBeforeSavedDays
    , afterSavedDays = dbAfterSavedDays
    }
  where
    dbSelect i = do
        row <- liftIO $ do
            stmt <- DB.prepare conn "SELECT * FROM entries WHERE id = ?"
            void $ DB.execute stmt [DB.toSql i]
            DB.fetchRow stmt
        maybe (throw RecordNotFound) (pure . fromSql) row

    dbSelectDay (DB.toSql -> day) = do
        rows <- liftIO $ do
            stmt <- DB.prepare conn "SELECT * FROM entries WHERE day = ? ORDER BY created_at ASC"
            void $ DB.execute stmt [day]
            DB.fetchAllRows stmt
        pure $ fromSql <$> rows

    dbAll :: MonadEntryDB m => m (Enumerator (Saved Entry) m a)
    dbAll = do
        rows <- liftIO $ do
            stmt <- DB.prepare conn "SELECT * FROM entries ORDER BY created_at DESC"
            void $ DB.execute stmt []
            DB.fetchAllRows stmt
        pure $ enumList 1 rows $= EL.map fromSql

    dbSearch (DB.toSql -> word) = do
        rows <- liftIO $ do
            stmt <- DB.prepare conn $
                   "SELECT * FROM entries "
                <> "WHERE title LIKE '%' || ? || '%' OR body LIKE '%' || ? || '%' "
                <> "ORDER BY id DESC"
            void $ DB.execute stmt [word, word]
            DB.fetchAllRows stmt
        pure $ enumList 1 rows $= EL.map fromSql

    dbInsert Entry {..} = do
        liftIO $ do
            now <- Ti.getZonedTime
            void $ DB.run conn
                "INSERT INTO entries (created_at, modified_at, day, title, body) VALUES (?, ?, ?, ?, ?)"
                [ DB.toSql now
                , DB.toSql now
                , DB.toSql $ zonedDay now
                , DB.toSql title
                , DB.toSql body
                ]
            DB.commit conn

    dbUpdate i Entry {..} = do
        liftIO $ do
            now <- Ti.getZonedTime
            void $ DB.run conn "UPDATE entries SET modified_at = ?, title = ?, body = ? WHERE id = ?"
                [ DB.toSql now
                , DB.toSql title
                , DB.toSql body
                , DB.toSql i
                ]
            DB.commit conn

    dbDelete i = do
        liftIO $ do
            status <- DB.run conn "DELETE FROM entries WHERE id = ?" [DB.toSql i]
            if status /= 1 then
                throw RecordNotFound
            else do
                DB.commit conn

    dbBeforeSavedDays (DB.toSql -> d) = do
        rows <- liftIO $ do
            stmt <- DB.prepare conn
                "SELECT day FROM entries WHERE day <= ? GROUP BY day ORDER BY day DESC"
            void $ DB.execute stmt [d]
            DB.fetchAllRows stmt
        pure $ enumList 1 rows $= EL.map (DB.fromSql . Prelude.head)

    dbAfterSavedDays (DB.toSql -> d) = do
        rows <- liftIO $ do
            stmt <- DB.prepare conn
                "SELECT day FROM entries WHERE day >= ? GROUP BY day ORDER BY day ASC"
            void $ DB.execute stmt [d]
            DB.fetchAllRows stmt
        pure $ enumList 1 rows $= EL.map (DB.fromSql . Prelude.head)

    fromSql [ DB.fromSql -> id_
            , DB.fromSql -> c_at
            , DB.fromSql -> m_at
            , _
            , DB.fromSql -> t
            , DB.fromSql -> b ] = Saved
            { idx = id_
            , createdAt = c_at
            , modifiedAt = m_at
            , refObject = Entry {title = t, body = b}
            }
    fromSql _ = undefined
