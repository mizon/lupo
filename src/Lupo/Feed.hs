{-# LANGUAGE OverloadedStrings #-}

module Lupo.Feed
  ( makeRecentFeed
  ) where

import Control.Applicative
import qualified Data.Text as T
import qualified Heist as H
import qualified Heist.Interpreted as HI
import Text.XmlHtml

import qualified Lupo.Database as LDB

makeRecentFeed :: Monad (H.HeistT m m) => [LDB.Saved LDB.Entry] -> H.Splice m
makeRecentFeed = HI.callTemplate "feed"
  [ ("lupo:index-path", )
  , ("lupo:last-updated", )
  , ("lupo:author", )
  , ("", )
  ]

makeEntryDigest :: H.Template -> T.Text
makeEntryDigest = T.intercalate " " . (extractText <$>)
  where
    extractText (TextNode txt) = txt
    extractText (Element _ _ es) = makeEntryDigest es
    extractText _ = T.empty
