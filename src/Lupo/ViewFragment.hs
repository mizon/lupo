{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.ViewFragment (
    renderBody
  , entryInfo
  , daySummary
  , dayTitle
  , anEntry
  , comments
  , emptyMonth
  , searchResult
  , monthNavigation
  , singleDayNavigation
  , multiDaysNavigation
  ) where

import Data.Monoid
import qualified Data.Time as Time
import qualified Data.Text as T
import Snap
import Text.Shakespeare.Text hiding (toText)
import qualified Text.Templating.Heist as H
import Text.XmlHtml

import qualified Lupo.Database as LDB
import qualified Lupo.Locale as LL
import qualified Lupo.Navigation as N
import qualified Lupo.Syntax as S
import Lupo.Util

renderBody :: LDB.Entry -> H.Template
renderBody LDB.Entry {..} = S.renderBody entryBody

entryInfo :: LDB.Saved LDB.Entry -> H.Template
entryInfo LDB.Saved {..} = pure $
  Element "tr" [] [
    Element "td" [("class", "date")] [TextNode $ timeToText createdAt]
  , Element "td" [] [TextNode $ LDB.entryTitle savedContent]
  , Element "td" [("class", "operation")] [
      Element "a" [("href", [st|/admin/#{toText idx}/edit|])] [TextNode "Edit"]
    , TextNode " "
    , Element "a" [
        ("href", [st|/admin/#{toText idx}/delete|])
      , ("onclick", "return confirm(\"Are you sure?\")")
      ] [TextNode "Delete"]
    ]
  ]

daySummary :: (Functor m, Monad m, LL.HasLocalizer (H.HeistT m)) => LDB.Day -> H.Splice m
daySummary LDB.Day {..} = H.callTemplate "_day-summary" [
    ("lupo:day-title", pure $ dayTitle day)
  , ("lupo:day-entries", pure $ anEntry =<< dayEntries)
  , ("lupo:link-to-comment", H.textSplice linkToComment)
  , ("lupo:comment-label", H.textSplice =<< commentLabel)
  ]
  where
    commentLabel
      | numOfComments > 0 = do
          label <- LL.localize "Comment"
          pure [st|#{label} (#{toText numOfComments})|]
      | otherwise = LL.localize "New Comment"

    linkToComment = [st|#{formatTime "/%Y%m%d" day}##{postedOrNew}|]
      where
        postedOrNew :: T.Text
        postedOrNew
          | numOfComments > 0 = "comments"
          | otherwise = "new-comment"

dayTitle :: Time.Day -> H.Template
dayTitle d = pure $ Element "a" [("href", dayLinkFormat d)] [TextNode $ dayFormat d]
  where
    dayFormat = formatTime "%Y-%m-%d"
    dayLinkFormat = formatTime "/%Y%m%d"

anEntry :: LDB.Saved LDB.Entry -> H.Template
anEntry LDB.Saved {..} =
     Element "h3" [] [TextNode $ LDB.entryTitle savedContent]
   : S.renderBody (LDB.entryBody savedContent)
  <> [Element "p" [("class", "time")] [TextNode $ formatTime "(%H:%M)" createdAt]]

comments :: (Monad m, LL.HasLocalizer (H.HeistT m)) => [LDB.Saved LDB.Comment] -> H.Splice m
comments [] = pure []
comments cs = H.callTemplate "_comments" [
    ("lupo:comments-caption", H.textSplice =<< LL.localize "Comments")
  , ("lupo:comments", H.mapSplices commentSplice cs)
  ]
  where
    commentSplice LDB.Saved {..} = H.callTemplateWithText "_comment" [
        ("lupo:comment-name", LDB.commentName savedContent)
      , ("lupo:comment-time", formatTime "%Y-%m-%d %H:%M" $ createdAt)
      , ("lupo:comment-content", LDB.commentBody savedContent)
      ]

emptyMonth :: LL.HasLocalizer m => m H.Template
emptyMonth = do
  message <- LL.localize "no this month entries"
  pure [Element "p" [("class", "empty-month")] [TextNode message]]

searchResult :: [LDB.Saved LDB.Entry] -> H.Template
searchResult es = [Element "table" [("id", "search-result")] (row <$> es)]
  where
    row LDB.Saved {..} =
      Element "tr" [] [
        Element "th" [("class", "result-day")] [TextNode $ timeToText createdAt]
      , Element "th" [("class", "result-title")] [TextNode $ LDB.entryTitle savedContent]
      , Element "td" [] [TextNode $ T.take 30 $ LDB.entryBody savedContent]
      ]

monthNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m))
                => N.Navigation (H.HeistT m) -> H.Splice m
monthNavigation nav = do
  newest <- newestElement
  previous <- N.getPreviousMonth nav
  next <- N.getNextMonth nav
  previousLabel <- LL.localize "Previous Month"
  nextLabel <- LL.localize "Next Month"
  H.callTemplate "_navigation" [
      ("lupo:previous-link", pure [mkMonthLink previousLabel previous])
    , ("lupo:middle-link", pure [newest])
    , ("lupo:next-link", pure [mkMonthLink nextLabel next])
    ]
  where
    mkMonthLink body m = Element "a" [("href", formatMonthLink m)] [TextNode body]
      where
        formatMonthLink = formatTime "/%Y%m"

singleDayNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m))
                    => N.Navigation (H.HeistT m) -> H.Splice m
singleDayNavigation nav = do
  previous <- N.getPreviousDay nav
  next <- N.getNextDay nav
  previousLabel <- LL.localize "Previous Day"
  thisMonthLabel <- LL.localize "This Month"
  nextLabel <- LL.localize "Next Day"
  H.callTemplate "_navigation" [
      ("lupo:previous-link", pure [mkDayLink previousLabel previous])
    , ("lupo:middle-link", pure [thisMonthLink thisMonthLabel])
    , ("lupo:next-link", pure [mkDayLink nextLabel next])
    ]
  where
    mkDayLink body = maybe (TextNode body) $ \d_ ->
      Element "a"
              [("href", formatTime "/%Y%m%d" d_)]
              [TextNode body]

    thisMonthLink body =
      Element "a"
              [("href", formatTime "/%Y%m" $ N.getThisMonth nav)]
              [TextNode body]

multiDaysNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m))
                    => Integer -> N.Navigation (H.HeistT m) -> H.Splice m
multiDaysNavigation nDays nav = do
  previous <- N.getPreviousPageTop nav nDays
  next <- N.getNextPageTop nav nDays
  previousLabel <- LL.localize "Previous %d Days"
  nextLabel <- LL.localize "Next %d Days"
  newest <- newestElement
  H.callTemplate "_navigation" [
      ("lupo:previous-link", pure [mkDayLink previousLabel previous])
    , ("lupo:middle-link", pure [newest])
    , ("lupo:next-link", pure [mkDayLink nextLabel next])
    ]
  where
    mkDayLink body = maybe (TextNode formattedBody) $ \d_ ->
      Element "a"
              [("href", [st|#{formatTime "/%Y%m%d-" d_}#{textNDays}|])]
              [TextNode formattedBody]
      where
        formattedBody = T.replace "%d" textNDays body
        textNDays = toText nDays

timeToText :: Time.ZonedTime -> T.Text
timeToText = formatTime "%Y-%m-%d"

newestElement :: LL.HasLocalizer m => m Node
newestElement = do
  label <- LL.localize "Newest"
  pure $ Element "a" [("href", "/")] [TextNode label]
