{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Backend.Entry
  ( makeEntryDatabase
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans
import Control.Monad.Writer
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Internal as EI
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time
import qualified Database.HDBC as DB
import Prelude hiding (all)

import Lupo.Entry
import Lupo.Exception
import qualified Lupo.FieldValidator as FV
import Lupo.Util

makeEntryDatabase :: DB.IConnection conn => conn -> (Comment -> Bool) -> IO EDBWrapper
makeEntryDatabase conn spamFilter = do
  selectStatement <- prepareMutexStatement "SELECT * FROM entries WHERE id = ?"
  selectAllStatement <- prepareMutexStatement "SELECT * FROM entries ORDER BY created_at DESC"
  selectByDayStatement <- prepareMutexStatement "SELECT * FROM entries WHERE day = ? ORDER BY created_at ASC"
  selectCommentsStatement <- prepareMutexStatement "SELECT * FROM comments WHERE day = ? ORDER BY created_at ASC"
  searchStatement <- prepareMutexStatement "SELECT * FROM entries WHERE title LIKE '%' || ? || '%' OR body LIKE '%' || ? || '%' ORDER BY id DESC"
  insertStatement <- prepareMutexStatement "INSERT INTO entries (created_at, modified_at, day, title, body) VALUES (?, ?, ?, ?, ?)"
  updateStatement <- prepareMutexStatement "UPDATE entries SET modified_at = ?, title = ?, body = ? WHERE id = ?"
  deleteStatement <- prepareMutexStatement "DELETE FROM entries WHERE id = ?"
  beforeSavedDaysStatement <- prepareMutexStatement "SELECT day FROM entries WHERE day <= ? GROUP BY day ORDER BY day DESC"
  afterSavedDaysStatement <- prepareMutexStatement "SELECT day FROM entries WHERE day >= ? GROUP BY day ORDER BY day ASC"
  insertCommentStatement <- prepareMutexStatement "INSERT INTO comments (created_at, modified_at, day, name, body) VALUES (?, ?, ?, ?, ?)"
  pure $ EDBWrapper EntryDatabase
    { selectOne = \(DB.toSql -> id') ->
        withTransactionGeneric conn $
          withMutexStatement selectStatement $ \stmt ->
            liftIO $ do
              void $ DB.execute stmt [id']
              row <- DB.fetchRow stmt
              maybe (throw RecordNotFound) (pure . sqlToEntry) row

    , selectAll = enumStatement conn selectAllStatement [] E.$= EL.map sqlToEntry

    , selectPage = \d@(DB.toSql -> sqlDay) ->
        withTransactionGeneric conn $ do
          entries <- E.run_ $ enumStatement conn selectByDayStatement [sqlDay] E.$= EL.map sqlToEntry E.$$ EL.consume
          comments <- E.run_ $ enumStatement conn selectCommentsStatement [sqlDay] E.$= EL.map sqlToComment E.$$ EL.consume
          pure $ makePage d entries comments

    , search = \(DB.toSql -> word) ->
        enumStatement conn searchStatement [word, word] E.$= EL.map sqlToEntry

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

      commentValidator = FV.makeFieldValidator $ \c@Comment {..} -> do
        FV.checkIsEmtpy commentName "Name"
        FV.checkIsTooLong commentName "Name"
        FV.checkIsEmtpy commentBody "Content"
        FV.checkIsTooLong commentBody "Content"
        unless (spamFilter c) $
          tell $ pure "Comment is invalid."

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

makePage :: Time.Day -> [Saved Entry] -> [Saved Comment] -> Page
makePage d es cs = Page
  { pageDay = d
  , pageEntries = es
  , pageComments = cs
  , numOfComments = Prelude.length cs
  }
