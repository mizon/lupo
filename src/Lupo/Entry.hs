{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lupo.Entry
  ( DatabaseContext
  , HasDatabase (..)
  , EntryDatabase (..)
  , EDBWrapper (..)
  , Page (..)
  , Saved (..)
  , Entry (..)
  , Comment (..)
  , getCreatedDay
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.CatchIO
import qualified Data.Enumerator as E
import qualified Data.Text as T
import qualified Data.Time as Time
import Prelude hiding (all)

import Lupo.Util

class (MonadCatchIO m, Applicative m, Functor m) => DatabaseContext m
instance (MonadCatchIO m, Applicative m, Functor m) => DatabaseContext m

class HasDatabase m where
  getDatabase :: DatabaseContext n => m (EntryDatabase n)

data EntryDatabase m = EntryDatabase
  { selectOne :: Integer -> m (Saved Entry)
  , selectAll :: forall a. E.Enumerator (Saved Entry) m a
  , selectPage :: Time.Day -> m Page
  , search :: T.Text -> forall a. E.Enumerator (Saved Entry) m a
  , insert :: Entry -> m ()
  , update :: Integer -> Entry -> m ()
  , delete :: Integer -> m ()
  , beforeSavedDays :: forall a. Time.Day -> E.Enumerator Time.Day m a
  , afterSavedDays :: forall a. Time.Day -> E.Enumerator Time.Day m a
  , insertComment :: Time.Day -> Comment -> m ()
  }

data EDBWrapper = EDBWrapper
  { unEDBWrapper :: DatabaseContext m => EntryDatabase m
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

getCreatedDay :: Saved a -> Time.Day
getCreatedDay = zonedDay . createdAt
