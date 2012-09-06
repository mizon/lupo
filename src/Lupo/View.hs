{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , FlexibleInstances
    , ViewPatterns
    , TemplateHaskell #-}
module Lupo.View
    ( Day(..)
    , emptyDay
    , entryBody
    , entryInfo
    , day
    , searchResult
    , monthNavigation
    , dayNavigation
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.Syntax as S
import qualified Lupo.Locale as LL
import Lupo.Application
import Snap
import Text.XmlHtml
import qualified Data.Time as Ti
import qualified Data.Enumerator.List as EL
import Data.Enumerator hiding (concatMap)
import qualified System.Locale as L
import qualified Text.Templating.Heist as H
import qualified Data.Text as T
import Data.Monoid

data Day a = Day
    { entriesDay :: Ti.Day
    , entries :: [EDB.Saved a]
    }

emptyDay :: Day a
emptyDay = Day undefined []

entryBody :: EDB.Entry -> [Node]
entryBody EDB.Entry {..} = S.renderBody body

entryInfo :: [EDB.Saved EDB.Entry] -> [Node]
entryInfo es = singleEntry <$> es
  where
    singleEntry EDB.Saved {refObject = EDB.Entry {..}, ..} =
        Element "tr" []
            [ Element "td" [("class", "date")] [TextNode $ timeToText createdAt]
            , Element "td" [] [TextNode title]
            , Element "td" [("class", "operation")]
                [ Element "a" [("href", "/admin/" <> toText idx <> "/edit")] [TextNode "Edit"]
                , TextNode " "
                , Element "a" [ ("href", "/admin/" <> toText idx <> "/delete")
                              , ("onclick", "return confirm(\"Are you sure?\")") ] [TextNode "Delete"]
                ]
            ]
      where
        toText = T.pack . show

timeToText :: Ti.ZonedTime -> T.Text
timeToText = T.pack . Ti.formatTime L.defaultTimeLocale "%Y-%m-%d"

day :: Day EDB.Entry -> Node
day Day {..} =
    Element "div" [("class", "day")] $
          (Element "h2" [] [Element "a" [("href", dayLinkFormat entriesDay)] [TextNode $ dayFormat entriesDay]])
        : concatMap anEntry entries
  where
    dayFormat = T.pack . Ti.formatTime L.defaultTimeLocale "%Y-%m-%d"
    dayLinkFormat = T.pack . Ti.formatTime L.defaultTimeLocale "/%Y%m%d"

    anEntry EDB.Saved {..} =
           Element "h3" [] [TextNode $ EDB.title refObject]
         : S.renderBody (EDB.body refObject)
        <> [Element "p" [("class", "time")] [TextNode $ timeFormat createdAt]]
      where
        timeFormat = T.pack . Ti.formatTime L.defaultTimeLocale "(%H:%M)"

searchResult :: [EDB.Saved EDB.Entry] -> [Node]
searchResult = (result <$>)
  where
    result EDB.Saved {..} = Element "tr" []
        [ Element "td" [] [TextNode $ timeToText createdAt]
        , Element "td" [] [TextNode $ EDB.title refObject]
        , Element "td" [] [TextNode $ T.take 30 $ EDB.body refObject]
        ]

monthNavigation :: Ti.Day -> H.Splice (Handler Lupo Lupo)
monthNavigation month = do
    previousLabel <- LL.localize "Previous Month"
    nextLabel <- LL.localize "Next Month"
    pure $
        [ Element "ul" [("class", "page-navigation")]
            [ Element "li" [] [mkMonthLink previousLabel $ previousMonth month]
            , Element "li" [] [mkMonthLink nextLabel $ nextMonth month]
            ]
        ]
  where
    mkMonthLink body m = Element "a" [("href", monthLinkFormat m)] [TextNode body]
      where
        monthLinkFormat = T.pack . Ti.formatTime L.defaultTimeLocale "/%Y%m"

    nextMonth (Ti.toGregorian -> (y, 12, _)) = Ti.fromGregorian (y + 1) 1 1
    nextMonth (Ti.toGregorian -> (y, m, _)) = Ti.fromGregorian y (m + 1) 1

    previousMonth (Ti.toGregorian -> (y, 1, _)) = Ti.fromGregorian (y - 1) 12 1
    previousMonth (Ti.toGregorian -> (y, m, _)) = Ti.fromGregorian y (m - 1) 1

dayNavigation :: Ti.Day -> H.Splice (Handler Lupo Lupo)
dayNavigation d = do
    previous <- getPreviousDay d
    next <- getNextDay d
    previousLabel <- LL.localize "Previous Day"
    thisMonthLabel <- LL.localize "This Month"
    nextLabel <- LL.localize "Next Day"
    pure $
        [ Element "ul" [("class", "page-navigation")]
            [ Element "li" [] [mkDayLink previousLabel previous]
            , Element "li" [] [thisMonthLink thisMonthLabel]
            , Element "li" [] [mkDayLink nextLabel next]
            ]
        ]
  where
    mkDayLink body = maybe (TextNode body) $ \d_ ->
        Element "a"
            [("href", T.pack $ Ti.formatTime L.defaultTimeLocale "/%Y%m%d" d_)]
            [TextNode body]

    thisMonthLink body = Element "a"
        [("href", T.pack $ Ti.formatTime L.defaultTimeLocale "/%Y%m" d)]
        [TextNode body]

    getNextDay (Ti.addDays 1 -> tommorow) = do
        db <- EDB.getEntryDB
        run_ =<< (EL.head >>==) <$> EDB.afterSavedDays db tommorow

    getPreviousDay (Ti.addDays (-1) -> yesterday) = do
        db <- EDB.getEntryDB
        run_ =<< (EL.head >>==) <$> EDB.beforeSavedDays db yesterday
