{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Database
  ( DatabaseContext
  , HasDatabase (..)
  , Day (..)
  , Saved (..)
  , Entry (..)
  , Comment (..)
  , Database (..)
  , getCreatedDay
  , makeDatabase
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.HDBC as DB
import Prelude hiding (all)

import Lupo.Exception
import qualified Lupo.FieldValidator as FV
import Lupo.Util

class (MonadCatchIO m, Applicative m, Functor m) => DatabaseContext m
instance (MonadCatchIO m, Applicative m, Functor m) => DatabaseContext m

class HasDatabase m where
  getDatabase :: DatabaseContext n => m (Database n)

data Day = Day
  { day :: Time.Day
  , dayEntries :: [Saved Entry]
  , numOfComments :: Int
  , dayComments :: [Saved Comment]
  } deriving (Eq, Show)

data Saved o = Saved
  { idx :: Integer
  , createdAt :: Time.ZonedTime
  , modifiedAt :: Time.ZonedTime
  , savedContent :: o
  } deriving Show

instance Eq o => Eq (Saved o) where
  self == other =
       idx self == idx other
    && savedContent self == savedContent other

data Entry = Entry
  { entryTitle :: T.Text
  , entryBody :: T.Text
  } deriving (Eq, Show)

data Comment = Comment
  { commentName :: T.Text
  , commentBody :: T.Text
  } deriving (Eq, Show)

data Database m = Database
  { select :: Integer -> m (Saved Entry)
  , selectDay :: Time.Day -> m Day
  , all :: forall a. m (Enumerator (Saved Entry) m a)
  , search :: forall a. T.Text -> m (Enumerator (Saved Entry) m a)
  , insert :: Entry -> m ()
  , update :: Integer -> Entry -> m ()
  , delete :: Integer -> m ()
  , beforeSavedDays :: forall a. Time.Day -> m (Enumerator Time.Day m a)
  , afterSavedDays :: forall a. Time.Day -> m (Enumerator Time.Day m a)
  , insertComment :: Time.Day -> Comment -> m ()
  }

getCreatedDay :: Saved a -> Time.Day
getCreatedDay = zonedDay . createdAt

makeDatabase :: (DB.IConnection conn, DatabaseContext m) => conn -> Database m
makeDatabase conn = Database
  { select = \i -> do
      row <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries WHERE id = ?"
        void $ DB.execute stmt [DB.toSql i]
        DB.fetchRow stmt
      maybe (throw RecordNotFound) (pure . sqlToEntry) row

  , selectDay = \d@(DB.toSql -> sqlDay) -> do
      entries <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries WHERE day = ? ORDER BY created_at ASC"
        void $ DB.execute stmt [sqlDay]
        (sqlToEntry <$>) <$> DB.fetchAllRows stmt
      comments <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM comments WHERE day = ? ORDER BY created_at ASC"
        void $ DB.execute stmt [sqlDay]
        (sqlToComment <$>) <$> DB.fetchAllRows stmt
      pure $ makeDay d entries comments

  , all = do
      rows <- liftIO $ do
        stmt <- DB.prepare conn "SELECT * FROM entries ORDER BY created_at DESC"
        void $ DB.execute stmt []
        DB.fetchAllRows stmt
      pure $ enumList 1 rows $= EL.map sqlToEntry

  , search = \(DB.toSql -> word) -> do
      rows <- liftIO $ do
        stmt <- DB.prepare conn $
             "SELECT * FROM entries "
          <> "WHERE title LIKE '%' || ? || '%' OR body LIKE '%' || ? || '%' "
          <> "ORDER BY id DESC"
        void $ DB.execute stmt [word, word]
        DB.fetchAllRows stmt
      pure $ enumList 1 rows $= EL.map sqlToEntry

  , insert = \Entry {..} -> do
      liftIO $ do
        let sql = "INSERT INTO entries (created_at, modified_at, day, title, body) "
               <> "VALUES (?, ?, ?, ?, ?)"
        now <- Time.getZonedTime
        void $ DB.run conn sql [
            DB.toSql now
          , DB.toSql now
          , DB.toSql $ zonedDay now
          , DB.toSql entryTitle
          , DB.toSql entryBody
          ]
        DB.commit conn

  , update = \i Entry {..} -> do
      liftIO $ do
        let sql = "UPDATE entries "
               <> "SET modified_at = ?, title = ?, body = ? "
               <> "WHERE id = ?"
        now <- Time.getZonedTime
        void $ DB.run conn sql
          [ DB.toSql now
          , DB.toSql entryTitle
          , DB.toSql entryBody
          , DB.toSql i
          ]
        DB.commit conn

  , delete = \i -> do
      liftIO $ do
        status <- DB.run conn "DELETE FROM entries WHERE id = ?" [DB.toSql i]
        if status /= 1 then
          throw RecordNotFound
        else do
          DB.commit conn

  , beforeSavedDays = \(DB.toSql -> d) -> do
      rows <- liftIO $ do
        stmt <- DB.prepare conn
          "SELECT day FROM entries WHERE day <= ? GROUP BY day ORDER BY day DESC"
        void $ DB.execute stmt [d]
        DB.fetchAllRows stmt
      pure $ enumList 1 rows $= EL.map (DB.fromSql . Prelude.head)

  , afterSavedDays = \(DB.toSql -> d) -> do
      rows <- liftIO $ do
        stmt <- DB.prepare conn
          "SELECT day FROM entries WHERE day >= ? GROUP BY day ORDER BY day ASC"
        void $ DB.execute stmt [d]
        DB.fetchAllRows stmt
      pure $ enumList 1 rows $= EL.map (DB.fromSql . Prelude.head)

  , insertComment = \d c@Comment {..} -> do
      FV.validate commentValidator c
      liftIO $ do
        now <- Time.getZonedTime
        void $ DB.run conn
          "INSERT INTO comments (created_at, modified_at, day, name, body) VALUES (?, ?, ?, ?, ?)"
            [ DB.toSql now
            , DB.toSql now
            , DB.toSql d
            , DB.toSql commentName
            , DB.toSql commentBody
            ]
        DB.commit conn
  }
  where
    sqlToEntry [ DB.fromSql -> id'
               , DB.fromSql -> c_at
               , DB.fromSql -> m_at
               , _
               , DB.fromSql -> t
               , DB.fromSql -> b
               ] = Saved
      { idx = id'
      , createdAt = c_at
      , modifiedAt = m_at
      , savedContent = Entry t b
      }
    sqlToEntry _ = error "in sql->entry conversion"

    sqlToComment [ DB.fromSql -> id'
                 , DB.fromSql -> c_at
                 , DB.fromSql -> m_at
                 , _
                 , DB.fromSql -> n
                 , DB.fromSql -> b
                 ] = Saved
      { idx = id'
      , createdAt = c_at
      , modifiedAt = m_at
      , savedContent = Comment n b
      }
    sqlToComment _ = error "in sql->comment conversion"

commentValidator :: FV.FieldValidator Comment
commentValidator = FV.makeFieldValidator $ \Comment {..} -> do
  FV.checkIsEmtpy commentName "Name"
  FV.checkIsTooLong commentName "Name"
  FV.checkIsEmtpy commentBody "Content"
  FV.checkIsTooLong commentBody "Content"

makeDay :: Time.Day -> [Saved Entry] -> [Saved Comment] -> Day
makeDay d es cs = Day
  { day = d
  , dayEntries = es
  , dayComments = cs
  , numOfComments = Prelude.length cs
  }
