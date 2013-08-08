{-# LANGUAGE NoOverloadedStrings #-}

module Lupo.Backends.URLMapper
  ( makeURLMapper
  ) where

import Control.Exception
import qualified Data.ByteString.Char8 as C
import Data.Function
import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Lens
import Text.Shakespeare.Text

import qualified Lupo.Entry as E
import Lupo.Import
import Lupo.URLMapper
import Lupo.Util

makeURLMapper :: Path -> URLMapper
makeURLMapper basePath = fix $ \self -> URLMapper
  { _entryPath = \s ->
      full $ "entries" </> show (s ^. E.idx)

  , _entryEditPath = \s ->
      full $ "admin" </> show (s ^. E.idx) </> "edit"

  , _entryDeletePath = \s ->
      full $ "admin" </> show (s ^. E.idx) </> "delete"

  , _singleDayPath = full . dayPath

  , _entryDayPath = \page e ->
     let base = self ^. singleDayPath (page ^. E.pageDay)
         n = maybe (assert False undefined) succ $ L.findIndex (== e) $ page ^. E.pageEntries
     in base <> C.pack ("#" <> over packed (T.justifyRight 2 '0') (show n))

  , _multiDaysPath = \d n ->
      full [st|#{dayPath d}-#{show n}|]

  , _monthPath = full . formatTime "%Y%m"
  , _topPagePath = full ""
  , _adminPath = full "admin"
  , _loginPath = full "login"
  , _initAccountPath = full "init-account"

  , _commentPostPath = \d ->
      full $ dayPath d </> "comment#new-comment"

  , _newCommentPath = \d ->
      full $ dayPath d <> "#new-comment"

  , _commentsPath = \d ->
      full $ dayPath d <> "#comments"

  , _cssPath = \css ->
      full $ "css" </> css

  , _feedPath = full "recent.atom"
  , _fullPath = full
  }
  where
    dayPath = toString . formatTime "%Y%m%d"
    full path = C.pack $ toString basePath </> toString path

    p </> c = toString p <> "/" <> toString c
    infixl 5 </>

class ToString a where
  toString :: a -> String
instance ToString String where
  toString = id
instance ToString C.ByteString where
  toString = C.unpack
instance ToString T.Text where
  toString = T.unpack
