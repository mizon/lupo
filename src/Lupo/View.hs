{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.View (
    DayView(..)
  , entryBody
  , entryInfo
  , dayView
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
import qualified Text.Templating.Heist as H
import Text.XmlHtml

import qualified Lupo.Database as LDB
import qualified Lupo.Locale as LL
import qualified Lupo.Navigation as N
import qualified Lupo.Syntax as S
import Lupo.Util

data DayView a = DayView {
    entriesDay :: Time.Day
  , entries :: [LDB.Saved a]
  }

entryBody :: LDB.Entry -> H.Template
entryBody LDB.Entry {..} = S.renderBody body

entryInfo :: LDB.Saved LDB.Entry -> Node
entryInfo LDB.Saved {refObject = LDB.Entry {..}, ..} =
  Element "tr" [] [
    Element "td" [("class", "date")] [TextNode $ timeToText createdAt]
  , Element "td" [] [TextNode title]
  , Element "td" [("class", "operation")] [
      Element "a" [("href", "/admin/" <> toText idx <> "/edit")] [TextNode "Edit"]
    , TextNode " "
    , Element "a" [
        ("href", "/admin/" <> toText idx <> "/delete")
      , ("onclick", "return confirm(\"Are you sure?\")")
      ] [TextNode "Delete"]
    ]
  ]

dayView :: DayView LDB.Entry -> Node
dayView DayView {..} =
  Element "div" [("class", "day")] $ dayTitle : (anEntry =<< entries)
  where
    dayTitle =
      Element "h2" [] [
        Element "a" [("href", dayLinkFormat entriesDay)] [
          TextNode $ dayFormat entriesDay
        ]
      ]
      where
        dayFormat = formatTime "%Y-%m-%d"
        dayLinkFormat = formatTime "/%Y%m%d"

    anEntry LDB.Saved {..} =
         Element "h3" [] [TextNode $ LDB.title refObject]
       : S.renderBody (LDB.body refObject)
      <> [Element "p" [("class", "time")] [TextNode $ formatTime "(%H:%M)" createdAt]]

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
      , Element "th" [("class", "result-title")] [TextNode $ LDB.title refObject]
      , Element "td" [] [TextNode $ T.take 30 $ LDB.body refObject]
      ]

monthNavigation :: (LDB.DatabaseContext m, LL.HasLocalizer m) => N.Navigation m -> m H.Template
monthNavigation nav = do
  newest <- newestElement
  previous <- N.getPreviousMonth nav
  next <- N.getNextMonth nav
  previousLabel <- LL.localize "Previous Month"
  nextLabel <- LL.localize "Next Month"
  pure [
      Element "ul" [("class", "page-navigation")] [
        Element "li" [] [mkMonthLink previousLabel previous]
      , newest
      , Element "li" [] [mkMonthLink nextLabel next]
      ]
    ]
  where
    mkMonthLink body m = Element "a" [("href", formatMonthLink m)] [TextNode body]
      where
        formatMonthLink = formatTime "/%Y%m"

singleDayNavigation :: (LDB.DatabaseContext m, LL.HasLocalizer m) => N.Navigation m -> m H.Template
singleDayNavigation nav = do
  previous <- N.getPreviousDay nav
  next <- N.getNextDay nav
  previousLabel <- LL.localize "Previous Day"
  thisMonthLabel <- LL.localize "This Month"
  nextLabel <- LL.localize "Next Day"
  pure [
      Element "ul" [("class", "page-navigation")] [
        Element "li" [] [mkDayLink previousLabel previous]
      , Element "li" [] [thisMonthLink thisMonthLabel]
      , Element "li" [] [mkDayLink nextLabel next]
      ]
    ]
  where
    mkDayLink body = maybe (TextNode body) $ \d_ ->
      Element "a" [
        ("href", formatTime "/%Y%m%d" d_)
      ] [TextNode body]

    thisMonthLink body =
      Element "a" [
        ("href", formatTime "/%Y%m" $ N.getThisMonth nav)
      ] [TextNode body]

multiDaysNavigation :: (LDB.DatabaseContext m, LL.HasLocalizer m) =>
  Integer -> N.Navigation m -> m H.Template
multiDaysNavigation nDays nav = do
  previous <- N.getPreviousPageTop nav nDays
  next <- N.getNextPageTop nav nDays
  previousLabel <- LL.localize "Previous %d Days"
  nextLabel <- LL.localize "Next %d Days"
  newest <- newestElement
  pure [
      Element "ul" [("class", "page-navigation")] [
        Element "li" [] [mkDayLink previousLabel previous]
      , newest
      , Element "li" [] [mkDayLink nextLabel next]
      ]
    ]
  where
    mkDayLink body = maybe (TextNode formattedBody) $ \d_ ->
      Element "a" [
        ("href", formatTime "/%Y%m%d-" d_ <> textNDays)
      ] [TextNode formattedBody]
      where
        formattedBody = T.replace "%d" textNDays body
        textNDays = toText nDays

timeToText :: Time.ZonedTime -> T.Text
timeToText = formatTime "%Y-%m-%d"

newestElement :: LL.HasLocalizer m => m Node
newestElement = do
  label <- LL.localize "Newest"
  pure $ Element "li" [] [Element "a" [("href", "/")] [TextNode label]]
