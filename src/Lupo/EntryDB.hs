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
    , EntryDB(..)
    , makeEntryDB
    ) where

import Lupo.Exception
import qualified Database.HDBC as DB
import qualified Data.Enumerator.Text as ET
import qualified Data.Enumerator.List as EL
import Data.Enumerator
import qualified Data.Time as Ti
import qualified Data.Text.Lazy as TL
import qualified Data.Text as T
import Control.Applicative
import Data.Functor
import Control.Monad
import Control.Monad.Trans
import Control.Monad.CatchIO
import System.FilePath
import System.IO
import Prelude hiding (all)

import Development.Placeholders

class (MonadCatchIO m, Applicative m, Functor m) => MonadEntryDB m where
    getEntryDB :: m EntryDB

data Entry = Entry
    { title :: TL.Text
    , body :: TL.Text
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
    maybe (throw RecordNotFound) fromSql row

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
    path <- entriesDir <$> getEntryDB
    liftIO $ do
        now <- Ti.getZonedTime
        void $ DB.run conn "INSERT INTO entries (created_at, modified_at, title) VALUES (?, ?, ?)"
            [ DB.toSql now
            , DB.toSql now
            , DB.toSql title
            ]
        stmt <- DB.prepare conn "SELECT max(id) FROM entries"
        void $ DB.execute stmt []
        Just [DB.fromSql -> (lastIdx :: Integer)] <- DB.fetchRow stmt
        DB.commit conn
        withFile (path </> show lastIdx) WriteMode $ \h ->
            run_ $ enumList 1 (TL.toChunks body) $$ ET.iterHandle h

dbDelete :: MonadEntryDB m => Integer -> m ()
dbDelete i = do
    conn <- connection <$> getEntryDB
    liftIO $ do
        status <- DB.run conn "DELETE FROM entries WHERE id = ?" [DB.toSql i]
        if status /= 1 then
            throw RecordNotFound
        else
            DB.commit conn

fromSql :: MonadEntryDB m => [DB.SqlValue] -> m (Saved Entry)
fromSql [ DB.fromSql -> id_
        , DB.fromSql -> c_at
        , DB.fromSql -> m_at
        , DB.fromSql -> t ] = do
    edir <- entriesDir <$> getEntryDB
    b <- liftIO $ run_ $ ET.enumFile (edir </> show id_) $$ ET.consume
    return Saved
        { idx = id_
        , createdAt = c_at
        , modifiedAt = m_at
        , refObject = Entry {title = t, body = b}
        }
fromSql _ = undefined
