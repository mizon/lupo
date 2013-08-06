{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Backends.Entry
  ( makeEntryDatabase
  ) where

import Control.Concurrent
import Control.Exception hiding (bracket, catch, throw)
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
import Prelude hiding (all)

import Lupo.Entry
import Lupo.Exception
import qualified Lupo.FieldValidator as FV
import Lupo.Import
import Lupo.Util

makeEntryDatabase :: DB.IConnection conn => conn -> (Comment -> Bool) -> IO EDBWrapper
makeEntryDatabase conn spamFilter = do
  pool <- makeStatementPool conn <$> newIORef M.empty
  pure $ EDBWrapper EntryDatabase
    { _selectOne = \(DB.toSql -> id') -> do
        row <- liftIO $ retryingTransaction conn $
          withStatement pool "SELECT * FROM entries WHERE id = ?" $ \stmt -> do
            void $ DB.execute stmt [id']
            DB.fetchRow stmt
        maybe (throw RecordNotFound) (pure . sqlToEntry) row

    , _selectAll = enumStatement pool "SELECT * FROM entries ORDER BY created_at DESC" [] E.$= EL.map sqlToEntry

    , _selectPage = \d@(DB.toSql -> sqlDay) ->
        retryingTransaction conn $ do
          entries <- E.run_ $ enumStatement pool "SELECT * FROM entries WHERE day = ? ORDER BY created_at ASC" [sqlDay]
                         E.$= EL.map sqlToEntry
                         E.$$ EL.consume
          comments <- E.run_ $ enumStatement pool "SELECT * FROM comments WHERE day = ? ORDER BY created_at ASC" [sqlDay]
                          E.$= EL.map sqlToComment
                          E.$$ EL.consume
          pure $ makePage d entries comments

    , _search = \(DB.toSql -> word) ->
        enumStatement pool "SELECT * FROM entries WHERE title LIKE '%' || ? || '%' OR body LIKE '%' || ? || '%' ORDER BY id DESC" [word, word] E.$= EL.map sqlToEntry

    , _insert = \e ->
        liftIO $ retryingTransaction conn $
          withStatement pool "INSERT INTO entries (created_at, modified_at, day, title, body) VALUES (?, ?, ?, ?, ?)" $ \stmt -> do
            now <- Time.getZonedTime
            void $ DB.execute stmt
              [ DB.toSql now
              , DB.toSql now
              , DB.toSql $ zonedDay now
              , DB.toSql $ e ^. entryTitle
              , DB.toSql $ e ^. entryBody
              ]

    , _update = \i e ->
        liftIO $ retryingTransaction conn $
          withStatement pool "UPDATE entries SET modified_at = ?, title = ?, body = ? WHERE id = ?" $ \stmt -> do
            now <- Time.getZonedTime
            void $ DB.execute stmt
              [ DB.toSql now
              , DB.toSql $ e ^. entryTitle
              , DB.toSql $ e ^. entryBody
              , DB.toSql i
              ]

    , _delete = \(DB.toSql -> i) -> do
        status <- liftIO $ retryingTransaction conn $ do
          withStatement pool "DELETE FROM entries WHERE id = ?" $ \stmt ->
            DB.execute stmt [i]
        when (status /= 1) $
          throw RecordNotFound

    , _beforeSavedDays = \(DB.toSql -> d) ->
        enumStatement pool "SELECT day FROM entries WHERE day <= ? GROUP BY day ORDER BY day DESC" [d] E.$= EL.map (DB.fromSql . Prelude.head)

    , _afterSavedDays = \(DB.toSql -> d) ->
        enumStatement pool "SELECT day FROM entries WHERE day >= ? GROUP BY day ORDER BY day ASC" [d] E.$= EL.map (DB.fromSql . Prelude.head)

    , _insertComment = \d c -> do
        FV.validate commentValidator c
        liftIO $ retryingTransaction conn $ do
          withStatement pool "INSERT INTO comments (created_at, modified_at, day, name, body) VALUES (?, ?, ?, ?, ?)" $ \stmt -> do
            now <- Time.getZonedTime
            void $ DB.execute stmt
              [ DB.toSql now
              , DB.toSql now
              , DB.toSql d
              , DB.toSql $ c ^. commentName
              , DB.toSql $ c ^. commentBody
              ]
    }
  where
    enumStatement pool stmt values step = retryingTransaction conn $
      withStatement pool stmt $ \prepared -> do
        void $ liftIO $ DB.execute prepared values
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
      { _idx = id'
      , _createdAt = c_at
      , _modifiedAt = m_at
      , _savedContent = Comment n b
      }
    sqlToComment _ = error "in sql->comment conversion"

    commentValidator = FV.makeFieldValidator $ \c -> do
      FV.checkIsEmtpy (c ^. commentName) "Name"
      FV.checkIsTooLong (c ^. commentName) "Name"
      FV.checkIsEmtpy (c ^. commentBody) "Content"
      FV.checkIsTooLong (c ^. commentBody) "Content"
      unless (spamFilter c) $ tell $ pure "Comment is invalid."

data StatementPool = StatementPool
  { useStatement :: String -> IO DB.Statement
  }

withStatement :: MonadCatchIO m => StatementPool -> String -> (DB.Statement -> m a) -> m a
withStatement pool stmt = bracket getStatement finishStatement
  where
    getStatement = liftIO $ useStatement pool stmt
    finishStatement = liftIO . DB.finish

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
  { _idx = id'
  , _createdAt = c_at
  , _modifiedAt = m_at
  , _savedContent = Entry t b
  }
sqlToEntry _ = error "in sql->entry conversion"

retryingTransaction :: (Applicative m, MonadCatchIO m, DB.IConnection conn) => conn -> m a -> m a
retryingTransaction conn action = actionWithRetrying (3 :: Int) <* liftIO (DB.commit conn)
  where
    actionWithRetrying count = action `catch` \(e :: SomeException) -> do
      liftIO $ DB.rollback conn
      if count > 0 then do
        liftIO $ threadDelay delayTime
        actionWithRetrying $ pred count
      else
        throw e
      where
        delayTime = 1 * 10 ^ (6 :: Int)

makePage :: Time.Day -> [Saved Entry] -> [Saved Comment] -> Page
makePage d es cs = Page
  { _pageDay = d
  , _pageEntries = es
  , _pageComments = cs
  , _numOfComments = Prelude.length cs
  }
