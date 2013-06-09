{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Backend.Entry
  ( makeEntryDatabase
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception hiding (catch, throw)
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans
import Control.Monad.Writer
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Internal as EI
import qualified Data.Enumerator.List as EL
import Data.IORef
import qualified Data.Map as M
import qualified Data.Time as Time
import qualified Database.HDBC as DB
import Prelude hiding (all, catch)

import Lupo.Entry
import Lupo.Exception
import qualified Lupo.FieldValidator as FV
import Lupo.Util

makeEntryDatabase :: DB.IConnection conn => conn -> (Comment -> Bool) -> IO EDBWrapper
makeEntryDatabase conn spamFilter = do
  pool <- makeStatementPool conn <$> newIORef M.empty
  pure $ EDBWrapper EntryDatabase
    { selectOne = \(DB.toSql -> id') -> do
        row <- liftIO $ retryingTransaction conn $ do
          stmt <- useStatement pool "SELECT * FROM entries WHERE id = ?"
          void $ DB.execute stmt [id']
          DB.fetchRow stmt
        maybe (throw RecordNotFound) (pure . sqlToEntry) row

    , selectAll = enumStatement pool "SELECT * FROM entries ORDER BY created_at DESC" [] E.$= EL.map sqlToEntry

    , selectPage = \d@(DB.toSql -> sqlDay) ->
        retryingTransaction conn $ do
          entries <- E.run_ $ enumStatement pool "SELECT * FROM entries WHERE day = ? ORDER BY created_at ASC" [sqlDay]
                         E.$= EL.map sqlToEntry
                         E.$$ EL.consume
          comments <- E.run_ $ enumStatement pool "SELECT * FROM comments WHERE day = ? ORDER BY created_at ASC" [sqlDay]
                          E.$= EL.map sqlToComment
                          E.$$ EL.consume
          pure $ makePage d entries comments

    , search = \(DB.toSql -> word) ->
        enumStatement pool "SELECT * FROM entries WHERE title LIKE '%' || ? || '%' OR body LIKE '%' || ? || '%' ORDER BY id DESC" [word, word] E.$= EL.map sqlToEntry

    , insert = \Entry {..} ->
        liftIO $ retryingTransaction conn $ do
          stmt <- useStatement pool "INSERT INTO entries (created_at, modified_at, day, title, body) VALUES (?, ?, ?, ?, ?)"
          now <- Time.getZonedTime
          void $ DB.execute stmt
            [ DB.toSql now
            , DB.toSql now
            , DB.toSql $ zonedDay now
            , DB.toSql entryTitle
            , DB.toSql entryBody
            ]

    , update = \i Entry {..} ->
        liftIO $ retryingTransaction conn $ do
          stmt <- useStatement pool "UPDATE entries SET modified_at = ?, title = ?, body = ? WHERE id = ?"
          now <- Time.getZonedTime
          void $ DB.execute stmt
            [ DB.toSql now
            , DB.toSql entryTitle
            , DB.toSql entryBody
            , DB.toSql i
            ]

    , delete = \(DB.toSql -> i) -> do
        status <- liftIO $ retryingTransaction conn $ do
          stmt <- useStatement pool "DELETE FROM entries WHERE id = ?"
          DB.execute stmt [i]
        when (status /= 1) $
          throw RecordNotFound

    , beforeSavedDays = \(DB.toSql -> d) ->
        enumStatement pool "SELECT day FROM entries WHERE day <= ? GROUP BY day ORDER BY day DESC" [d] E.$= EL.map (DB.fromSql . Prelude.head)

    , afterSavedDays = \(DB.toSql -> d) ->
        enumStatement pool "SELECT day FROM entries WHERE day >= ? GROUP BY day ORDER BY day ASC" [d] E.$= EL.map (DB.fromSql . Prelude.head)

    , insertComment = \d c@Comment {..} -> do
        FV.validate commentValidator c
        liftIO $ retryingTransaction conn $ do
          stmt <- useStatement pool "INSERT INTO comments (created_at, modified_at, day, name, body) VALUES (?, ?, ?, ?, ?)"
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
    enumStatement pool stmt values step =  do
      prepared <- retryingTransaction conn $ do
        prepared' <- liftIO $ useStatement pool stmt
        void $ liftIO $ DB.execute prepared' values
        pure prepared'
      loop prepared step
      where
        loop prepared (E.Continue f) = do
          e <- liftIO $ DB.fetchRow prepared
          loop prepared E.==<< f (maybe E.EOF (E.Chunks . pure) e)
        loop _ s = EI.returnI s

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
      unless (spamFilter c) $ tell $ pure "Comment is invalid."

data StatementPool = StatementPool
  { useStatement :: String -> IO DB.Statement
  }

makeStatementPool :: DB.IConnection conn => conn -> IORef (M.Map String DB.Statement) -> StatementPool
makeStatementPool conn statements = StatementPool doUseStatement
  where
    doUseStatement stmt = do
      stmts <- readIORef statements
      case M.lookup stmt stmts of
        Just prepared -> pure prepared
        Nothing -> do
          prepared <- DB.prepare conn stmt
          statements `modifyIORef` M.insert stmt prepared
          doUseStatement stmt

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

retryingTransaction :: (Applicative m, MonadCatchIO m, DB.IConnection conn) => conn -> m a -> m a
retryingTransaction conn action = actionWithRetrying 3 <* liftIO (DB.commit conn)
  where
    actionWithRetrying count = action `catch` \(e :: SomeException) -> do
      liftIO $ print e
      liftIO $ DB.rollback conn
      if count > 0 then do
        liftIO $ threadDelay delayTime
        actionWithRetrying $ pred count
      else do
        throw e
      where
        delayTime = 1 * 10 ^ 6

makePage :: Time.Day -> [Saved Entry] -> [Saved Comment] -> Page
makePage d es cs = Page
  { pageDay = d
  , pageEntries = es
  , pageComments = cs
  , numOfComments = Prelude.length cs
  }
