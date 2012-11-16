{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.ViewFragment (
    renderBody
  , entryInfo
  , daySummary
  , dayTitle
  , anEntry
  , comment
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
entryInfo LDB.Saved {refObject = LDB.Entry {..}, ..} = pure $
  Element "tr" [] [
    Element "td" [("class", "date")] [TextNode $ timeToText createdAt]
  , Element "td" [] [TextNode entryTitle]
  , Element "td" [("class", "operation")] [
      Element "a" [("href", [st|/admin/#{toText idx}/edit|])] [TextNode "Edit"]
    , TextNode " "
    , Element "a" [
        ("href", [st|/admin/#{toText idx}/delete|])
      , ("onclick", "return confirm(\"Are you sure?\")")
      ] [TextNode "Delete"]
    ]
  ]

daySummary :: LDB.Day -> H.Template
daySummary LDB.Day {..} = pure $
  Element "div" [("class", "day")] $
       dayTitle day
    <> (anEntry =<< dayEntries)
    <> commentsSummary
  where
    commentsSummary
      | numOfComments > 0 = [
          Element "p" [] [TextNode [st|Comments (#{toText numOfComments})|]]
        ]
      | otherwise = [
          Element "p" [] [TextNode "Comment"]
        ]

dayTitle :: Time.Day -> H.Template
dayTitle d = pure $
  Element "h2" [] [
    Element "a" [("href", dayLinkFormat d)] [
      TextNode $ dayFormat d
    ]
  ]
  where
    dayFormat = formatTime "%Y-%m-%d"
    dayLinkFormat = formatTime "/%Y%m%d"

anEntry :: LDB.Saved LDB.Entry -> H.Template
anEntry LDB.Saved {..} =
     Element "h3" [] [TextNode $ LDB.entryTitle refObject]
   : S.renderBody (LDB.entryBody refObject)
  <> [Element "p" [("class", "time")] [TextNode $ formatTime "(%H:%M)" createdAt]]

comment :: LDB.Saved LDB.Comment -> H.Template
comment LDB.Saved {..} = [
    Element "dt" [] [
      TextNode $ LDB.commentName refObject
    ]
  , Element "dd" [] [
      TextNode $ LDB.commentBody refObject
    ]
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
      , Element "th" [("class", "result-title")] [TextNode $ LDB.entryTitle refObject]
      , Element "td" [] [TextNode $ T.take 30 $ LDB.entryBody refObject]
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

singleDayNavigation :: LL.HasLocalizer m => N.Navigation m -> m H.Template
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
