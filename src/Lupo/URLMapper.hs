module Lupo.URLMapper
  ( HasURLMapper (..)
  , URLMapper (..)
  , Path
  , getURL
  , toURLSplice
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Heist.Interpreted as H

import qualified Lupo.Entry as E

class Functor m => HasURLMapper m where
  getURLMapper :: m URLMapper

data URLMapper = URLMapper
  { entryPath :: E.Saved E.Entry -> Path
  , entryEditPath :: E.Saved E.Entry -> Path
  , singleDayPath :: Time.Day -> Path
  , multiDaysPath :: Time.Day -> Int -> Path
  , monthPath :: Time.Day -> Path
  , topPagePath :: Path
  , adminPath :: Path
  , loginPath :: Path
  , initAccountPath :: Path
  , commentPostPath :: Time.Day -> Path
  , newCommentPath :: Time.Day -> Path
  , commentsPath :: Time.Day -> Path
  , cssPath :: BS.ByteString -> Path
  , fullPath :: Path -> Path
  }

type Path = BS.ByteString

getURL :: HasURLMapper m => (URLMapper -> a) -> m a
getURL = (<$> getURLMapper)

toURLSplice :: Monad m => Path -> H.Splice m
toURLSplice = H.textSplice . Encoding.decodeUtf8
