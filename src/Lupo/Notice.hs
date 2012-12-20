{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.Notice
  ( NoticeDB (..)
  , SessionBackend (..)
  , makeNoticeDB
  , makeSessionBackend
  ) where

import Control.Applicative
import qualified Data.Text as T
import qualified Database.HDBC as DB
import Snap
import qualified Snap.Snaplet.Session as SS

data NoticeDB m = NoticeDB
  { addNotice :: T.Text -> m ()
  , popAllNotice :: m [T.Text]
  }

data SessionBackend m = SessionBackend
  { getCsrfToken :: m T.Text
  , commitSession :: m ()
  }

makeNoticeDB :: (DB.IConnection c, Applicative m, MonadIO m)
             => c -> SessionBackend m -> NoticeDB m
makeNoticeDB conn SessionBackend {..} = NoticeDB
  { addNotice = \(DB.toSql -> msg) -> do
      (DB.toSql -> token) <- getCsrfToken
      commitSession
      liftIO $ do
        void $ DB.run conn "INSERT INTO notice (token, message) VALUES (?, ?)"
          [ token
          , msg
          ]
        DB.commit conn

  , popAllNotice = do
      (DB.toSql -> token) <- getCsrfToken
      messages <- liftIO $ DB.withTransaction conn $ const $ do
        stmt <- DB.prepare conn "SELECT message FROM notice WHERE token = ?"
        void $ DB.execute stmt [token]
        (concat -> messages') <- DB.fetchAllRows' stmt
        void $ DB.run conn "DELETE FROM notice WHERE token = ?" [token]
        pure messages'
      liftIO $ DB.commit conn
      pure $ DB.fromSql <$> messages
  }

makeSessionBackend :: SnapletLens v SS.SessionManager -> SessionBackend (Handler b v)
makeSessionBackend session = SessionBackend
  { getCsrfToken = with session SS.csrfToken
  , commitSession = with session SS.commitSession
  }
