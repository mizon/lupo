{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Entry
  ( EntryDatabase (..)
  , makeEntryDatabase
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Internal as EI
import qualified Data.Text as T
import qualified Data.Time as Time
import qualified Database.HDBC as DB
import Prelude hiding (all)

import Lupo.Exception

data EntryDatabase m = EntryDatabase
  { select :: Integer -> m (Saved Entry)
  , all :: forall a. E.Enumerator (Saved Entry) m a
  }

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

class (MonadCatchIO m, Applicative m, Functor m) => DatabaseContext m
instance (MonadCatchIO m, Applicative m, Functor m) => DatabaseContext m

data Entry = Entry
  { entryTitle :: T.Text
  , entryBody :: T.Text
  } deriving (Eq, Show)

data Comment = Comment
  { commentName :: T.Text
  , commentBody :: T.Text
  } deriving (Eq, Show)

makeEntryDatabase :: (DB.IConnection conn, DatabaseContext m) => conn -> IO (EntryDatabase m)
makeEntryDatabase conn = do
  selectStatement <- DB.prepare conn "SELECT * FROM entries WHERE id = ?"
  getAllStatement <- DB.prepare conn "SELECT * FROM entries ORDER BY created_at DESC"
  pure EntryDatabase
    { select = \(DB.toSql -> id') -> liftIO $ DB.withTransaction conn $ const $ do
        void $ DB.execute selectStatement [id']
        row <- DB.fetchRow selectStatement
        DB.finish selectStatement
        maybe (throw RecordNotFound) (pure . sqlToEntry) row

    , all = getAllEntries conn getAllStatement
    }

getAllEntries :: (DB.IConnection conn, MonadIO m) => conn -> DB.Statement -> E.Enumerator (Saved Entry) m a
getAllEntries conn stmt step = do
  void $ liftIO $ DB.execute stmt []
  loop step
  where
    loop (E.Continue f) = do
      e <- liftIO $ (sqlToEntry <$>) <$> DB.fetchRow stmt
      case e of
        Just e' -> loop E.==<< f (E.Chunks [e'])
        Nothing -> f E.EOF
    loop (E.Yield b s) = finalize *> EI.yield b s
    loop s = finalize *> EI.returnI s

    finalize = liftIO $ do
      DB.finish stmt
      DB.commit conn

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
