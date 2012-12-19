{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.URLMapper
  ( HasURLMapper (..)
  , URLMapper (..)
  , Path
  , getURL
  , urlSplice
  , makeURLMapper
  ) where

import Control.Applicative
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import Text.Shakespeare.Text
import qualified Text.Templating.Heist as H

import qualified Lupo.Database as LDB
import Lupo.Util

class Functor m => HasURLMapper m where
  getURLMapper :: m URLMapper

data URLMapper = URLMapper
  { entryPath :: LDB.Saved LDB.Entry -> Path
  , entryEditPath :: LDB.Saved LDB.Entry -> Path
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

getURL :: HasURLMapper m => (URLMapper -> Path) -> m Path
getURL = (<$> getURLMapper)

urlSplice :: (HasURLMapper (H.HeistT m), Monad m) => (URLMapper -> BS.ByteString) -> H.Splice m
urlSplice f = H.textSplice =<< Encoding.decodeUtf8 <$> getURL f

makeURLMapper :: Path -> URLMapper
makeURLMapper basePath = URLMapper
  { entryPath = \LDB.Saved {..} -> fullPath' $ "entries" </> show idx
  , entryEditPath = \LDB.Saved {..} -> fullPath' $ "admin" </> show idx </> "edit"
  , singleDayPath = fullPath' . dayPath
  , multiDaysPath = \d n -> fullPath' $ T.unpack [st|#{dayPath d}-#{show n}|]
  , monthPath = fullPath' . T.unpack . formatTime "%Y%m"
  , topPagePath = fullPath' ""
  , adminPath = fullPath' "admin"
  , loginPath = fullPath' "login"
  , initAccountPath = fullPath' "init-account"
  , commentPostPath = \d -> fullPath' $ dayPath d </> "comment#new-comment"
  , newCommentPath = \d -> fullPath' $ dayPath d <> "#new-comment"
  , commentsPath = \d -> fullPath' $ dayPath d <> "#comments"
  , cssPath = \(BS.unpack -> css) -> fullPath' $ "css" </> css
  , fullPath = \(BS.unpack -> path) -> fullPath' path
  }
  where
    dayPath = T.unpack . formatTime "%Y%m%d"
    fullPath' = BS.pack . (BS.unpack basePath </>)

    p </> c = p <> "/" <> c
    infixl 5 </>
