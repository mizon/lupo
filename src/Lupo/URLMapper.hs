module Lupo.URLMapper
  ( HasURLMapper (..)
  , URLMapper (..)
  , entryPath
  , entryEditPath
  , entryDeletePath
  , singleDayPath
  , entryDayPath
  , multiDaysPath
  , monthPath
  , topPagePath
  , adminPath
  , loginPath
  , initAccountPath
  , commentPostPath
  , newCommentPath
  , commentsPath
  , cssPath
  , fullPath
  , Path
  , getURL
  , urlSplice
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import qualified Heist as H
import qualified Heist.Interpreted as H

import qualified Lupo.Entry as E
import Lupo.Import

class Functor m => HasURLMapper m where
  getURLMapper :: m URLMapper

data URLMapper = URLMapper
  { _entryPath :: E.Saved E.Entry -> Path
  , _entryEditPath :: E.Saved E.Entry -> Path
  , _entryDeletePath :: E.Saved E.Entry -> Path
  , _singleDayPath :: Time.Day -> Path
  , _entryDayPath :: E.Page -> E.Saved E.Entry -> Path
  , _multiDaysPath :: Time.Day -> Int -> Path
  , _monthPath :: Time.Day -> Path
  , _topPagePath :: Path
  , _adminPath :: Path
  , _loginPath :: Path
  , _initAccountPath :: Path
  , _commentPostPath :: Time.Day -> Path
  , _newCommentPath :: Time.Day -> Path
  , _commentsPath :: Time.Day -> Path
  , _cssPath :: BS.ByteString -> Path
  , _fullPath :: Path -> Path
  }

entryPath :: E.Saved E.Entry -> Getter URLMapper Path
entryPath e = to $ \self ->
  _entryPath self e

entryEditPath :: E.Saved E.Entry -> Getter URLMapper Path
entryEditPath e = to $ \self ->
  _entryEditPath self e

entryDeletePath :: E.Saved E.Entry -> Getter URLMapper Path
entryDeletePath e = to $ \self ->
  _entryDeletePath self e

singleDayPath :: Time.Day -> Getter URLMapper Path
singleDayPath d = to $ \self ->
  _singleDayPath self d

entryDayPath :: E.Page -> E.Saved E.Entry -> Getter URLMapper Path
entryDayPath p e = to $ \self ->
  _entryDayPath self p e

multiDaysPath :: Time.Day -> Int -> Getter URLMapper Path
multiDaysPath d n = to $ \self ->
  _multiDaysPath self d n

monthPath :: Time.Day -> Getter URLMapper Path
monthPath d = to $ \self ->
  _monthPath self d

topPagePath :: Getter URLMapper Path
topPagePath = to _topPagePath

adminPath :: Getter URLMapper Path
adminPath = to _adminPath

loginPath :: Getter URLMapper Path
loginPath = to _loginPath

initAccountPath :: Getter URLMapper Path
initAccountPath = to _initAccountPath

commentPostPath :: Time.Day -> Getter URLMapper Path
commentPostPath d = to $ \self ->
  _commentPostPath self d

newCommentPath :: Time.Day -> Getter URLMapper Path
newCommentPath d = to $ \self ->
  _newCommentPath self d

commentsPath :: Time.Day -> Getter URLMapper Path
commentsPath d = to $ \self ->
  _commentsPath self d

cssPath :: BS.ByteString -> Getter URLMapper Path
cssPath css = to $ \self ->
  _cssPath self css

fullPath :: Path -> Getter URLMapper Path
fullPath path = to $ \self ->
  _fullPath self path

type Path = BS.ByteString

getURL :: (Monad m, HasURLMapper m) => Getter URLMapper Path -> m Path
getURL getter = getURLMapper <&> view getter

urlSplice :: (Monad m, HasURLMapper (H.HeistT m m)) => Getter URLMapper Path -> H.Splice m
urlSplice getter = do
  url <- getURL getter
  H.textSplice $ Encoding.decodeUtf8 url
