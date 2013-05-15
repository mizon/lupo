{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Backend.URLMapper
  ( makeURLMapper
  ) where

import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import qualified Data.Text as T
import Text.Shakespeare.Text

import Lupo.URLMapper
import qualified Lupo.Entry as E
import Lupo.Util

makeURLMapper :: Path -> URLMapper
makeURLMapper basePath = URLMapper
  { entryPath = \E.Saved {..} -> fullPath' $ "entries" </> show idx
  , entryEditPath = \E.Saved {..} -> fullPath' $ "admin" </> show idx </> "edit"
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
