{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Entry
  ( EntryDatabase (..)
  , Page (..)
  , Saved (..)
  , makeEntryDatabase
  , Entry (..)
  , Comment (..)
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Internal as EI
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

data EntryDatabase m = EntryDatabase
  { selectOne :: Integer -> m (Saved Entry)
  , selectAll :: forall a. E.Enumerator (Saved Entry) m a
  , insert :: Entry -> m ()
  , update :: Integer -> Entry -> m ()
  , delete :: Integer -> m ()
  , beforeSavedDays :: forall a. Time.Day -> E.Enumerator Time.Day m a
  , afterSavedDays :: forall a. Time.Day -> E.Enumerator Time.Day m a
  , insertComment :: Time.Day -> Comment -> m ()
  }

data Page = Page
  { pageDay :: Time.Day
  , pageEntries :: [Saved Entry]
  , numOfComments :: Int
  , pageComments :: [Saved Comment]
  } deriving (Eq, Show)

data Saved o = Saved
  { idx :: Integer
  , createdAt :: Time.ZonedTime
  , modifiedAt :: Time.ZonedTime
  , savedContent :: o
  } deriving Show

instance Eq o => Eq (Saved o) where
  self == other = idx self == idx other
               && savedContent self == savedContent other

data Entry = Entry
  { entryTitle :: T.Text
  , entryBody :: T.Text
  } deriving (Eq , Show)

data Comment = Comment
  { commentName :: T.Text
  , commentBody :: T.Text
  } deriving (Eq, Show)

makeEntryDatabase :: (DB.IConnection conn, DatabaseContext m) => conn -> IO (EntryDatabase m)
makeEntryDatabase conn = do
  selectStatement <- prepareMutexStatement "SELECT * FROM entries WHERE id = ?"
  selectAllStatement <- prepareMutexStatement "SELECT * FROM entries ORDER BY created_at DESC"
  insertStatement <- prepareMutexStatement "INSERT INTO entries (created_at, modified_at, day, title, body) VALUES (?, ?, ?, ?, ?)"
  updateStatement <- prepareMutexStatement "UPDATE entries SET modified_at = ?, title = ?, body = ? WHERE id = ?"
  deleteStatement <- prepareMutexStatement "DELETE FROM entries WHERE id = ?"
  beforeSavedDaysStatement <- prepareMutexStatement "SELECT day FROM entries WHERE day <= ? GROUP BY day ORDER BY day DESC"
  afterSavedDaysStatement <- prepareMutexStatement "SELECT day FROM entries WHERE day >= ? GROUP BY day ORDER BY day ASC"
  insertCommentStatement <- prepareMutexStatement "INSERT INTO comments (created_at, modified_at, day, name, body) VALUES (?, ?, ?, ?, ?)"
  pure EntryDatabase
    { selectOne = \(DB.toSql -> id') ->
        withTransactionGeneric conn $
          withMutexStatement selectStatement $ \stmt ->
            liftIO $ do
              void $ DB.execute stmt [id']
              row <- DB.fetchRow stmt
              maybe (throw RecordNotFound) (pure . sqlToEntry) row

    , selectAll = enumStatement conn selectAllStatement [] E.$= EL.map sqlToEntry

    , insert = \Entry {..} ->
        withTransactionGeneric conn $
          withMutexStatement insertStatement $ \stmt -> liftIO $ do
            now <- Time.getZonedTime
            void $ DB.execute stmt
              [ DB.toSql now
              , DB.toSql now
              , DB.toSql $ zonedDay now
              , DB.toSql entryTitle
              , DB.toSql entryBody
              ]

    , update = \i Entry {..} ->
        withTransactionGeneric conn $
          withMutexStatement updateStatement $ \stmt -> liftIO $ do
            now <- Time.getZonedTime
            void $ DB.execute stmt
              [ DB.toSql now
              , DB.toSql entryTitle
              , DB.toSql entryBody
              , DB.toSql i
              ]

    , delete = \(DB.toSql -> i) -> liftIO $
        withTransactionGeneric conn $
          withMutexStatement deleteStatement $ \stmt -> do
            status <- liftIO $ DB.execute stmt [i]
            when (status /= 1) $
              throw RecordNotFound

    , beforeSavedDays = \(DB.toSql -> d) ->
        enumStatement conn beforeSavedDaysStatement [d] E.$= EL.map (DB.fromSql . Prelude.head)

    , afterSavedDays = \(DB.toSql -> d) ->
        enumStatement conn afterSavedDaysStatement [d] E.$= EL.map (DB.fromSql . Prelude.head)

    , insertComment = \d c@Comment {..} -> do
        FV.validate commentValidator c
        withTransactionGeneric conn $
          withMutexStatement insertCommentStatement $ \stmt -> liftIO $ do
            now <- Time.getZonedTime
            void $ DB.execute stmt
              [ DB.toSql now
              , DB.toSql now
              , DB.toSql d
              , DB.toSql commentName
              , DB.toSql commentBody
              ]
    }
    where
      prepareMutexStatement = newMVar <=< DB.prepare conn

enumStatement :: (DB.IConnection conn, Functor m, MonadCatchIO m) => conn -> MVar DB.Statement -> [DB.SqlValue] -> E.Enumerator [DB.SqlValue] m a
enumStatement conn mutex values step =
  withTransactionGeneric conn $
    withMutexStatement mutex $ \stmt -> do
      void $ liftIO $ DB.execute stmt values
      loop stmt step
  where
    loop stmt (E.Continue f) = do
      e <- liftIO $ DB.fetchRow stmt
      loop stmt E.==<< f (maybe E.EOF (E.Chunks . pure) e)
    loop _ s = EI.returnI s

commentValidator :: FV.FieldValidator Comment
commentValidator = FV.makeFieldValidator $ \Comment {..} -> do
  FV.checkIsEmtpy commentName "Name"
  FV.checkIsTooLong commentName "Name"
  FV.checkIsEmtpy commentBody "Content"
  FV.checkIsTooLong commentBody "Content"

sqlToEntry :: [DB.SqlValue] -> Saved Entry
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

withTransactionGeneric :: (Applicative m, MonadCatchIO m, DB.IConnection conn) => conn -> m a -> m a
withTransactionGeneric conn action = onException action (liftIO $ DB.rollback conn)
                                  <* liftIO (DB.commit conn)

withMutexStatement :: MonadCatchIO m => MVar DB.Statement -> (DB.Statement -> m a) -> m a
withMutexStatement mutex = bracket takeStatement finishStatement
  where
    takeStatement = liftIO $ takeMVar mutex

    finishStatement stmt = liftIO $ do
      DB.finish stmt
      putMVar mutex stmt
