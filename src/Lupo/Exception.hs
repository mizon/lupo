{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}

module Lupo.Exception
  ( LupoException
  , RecordNotFound (..)
  , InvalidLocaleFile (..)
  , InvalidField (..)
  ) where

import Control.Exception
import qualified Data.Text as T
import Data.Typeable

data LupoException = forall e. Exception e => LupoException e
  deriving Typeable

instance Exception LupoException

instance Show LupoException where
  show (LupoException e) = show e

data RecordNotFound = RecordNotFound
  deriving (Typeable, Show)

instance Exception RecordNotFound where
  toException = toLupoException
  fromException = fromLupoException

data InvalidLocaleFile = InvalidLocaleFile
  deriving (Typeable, Show)

instance Exception InvalidLocaleFile where
  toException = toLupoException
  fromException = fromLupoException

data InvalidField = InvalidField [T.Text]
  deriving (Typeable, Show)

instance Exception InvalidField where
  toException = toLupoException
  fromException = fromLupoException

toLupoException :: Exception e => e -> SomeException
toLupoException = toException . LupoException

fromLupoException :: Exception e => SomeException -> Maybe e
fromLupoException e = do
  LupoException e1 <- fromException e
  cast e1
