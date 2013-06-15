{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Backends.URLMapper
  ( makeURLMapper
  ) where

import qualified Data.ByteString.Char8 as C
import Data.Monoid
import qualified Data.Text as T
import Text.Shakespeare.Text

import Lupo.URLMapper
import qualified Lupo.Entry as E
import Lupo.Util

makeURLMapper :: Path -> URLMapper
makeURLMapper basePath = URLMapper
  { entryPath = \E.Saved {..} ->
      full $ "entries" </> show idx

  , entryEditPath = \E.Saved {..} ->
      full $ "admin" </> show idx </> "edit"

  , singleDayPath = full . dayPath

  , multiDaysPath = \d n ->
      full [st|#{dayPath d}-#{show n}|]

  , monthPath = full . formatTime "%Y%m"
  , topPagePath = full ""
  , adminPath = full "admin"
  , loginPath = full "login"
  , initAccountPath = full "init-account"

  , commentPostPath = \d ->
      full $ dayPath d </> "comment#new-comment"

  , newCommentPath = \d ->
      full $ dayPath d <> "#new-comment"

  , commentsPath = \d ->
      full $ dayPath d <> "#comments"

  , cssPath = \css ->
      full $ "css" </> css

  , fullPath = full
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
