{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , FlexibleInstances
    , ViewPatterns
    , TemplateHaskell #-}
module Lupo.View
    ( DayView(..)
    , entryBody
    , entryInfo
    , dayView
    , searchResult
    , monthNavigation
    , dayNavigation
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.Syntax as S
import qualified Lupo.Locale as LL
import Lupo.Util
import Lupo.Application
import Snap
import Text.XmlHtml
import qualified Data.Time as Time
import qualified Data.Enumerator.List as EL
import Data.Enumerator hiding (concatMap)
import qualified System.Locale as L
import qualified Text.Templating.Heist as H
import qualified Data.Text as T
import Data.Monoid

data DayView a = DayView
    { entriesDay :: Time.Day
    , entries :: [EDB.Saved a]
    }

entryBody :: EDB.Entry -> [Node]
entryBody EDB.Entry {..} = S.renderBody body

entryInfo :: EDB.Saved EDB.Entry -> Node
entryInfo EDB.Saved {refObject = EDB.Entry {..}, ..} = Element "tr" []
    [ Element "td" [("class", "date")] [TextNode $ timeToText createdAt]
    , Element "td" [] [TextNode title]
    , Element "td" [("class", "operation")]
        [ Element "a" [("href", "/admin/" <> toText idx <> "/edit")] [TextNode "Edit"]
        , TextNode " "
        , Element "a" [ ("href", "/admin/" <> toText idx <> "/delete")
                      , ("onclick", "return confirm(\"Are you sure?\")") ] [TextNode "Delete"]
        ]
    ]

dayView :: DayView EDB.Entry -> Node
dayView DayView {..} =
    Element "div" [("class", "day")] $
          (Element "h2" [] [Element "a" [("href", dayLinkFormat entriesDay)] [TextNode $ dayFormat entriesDay]])
        : concatMap anEntry entries
  where
    dayFormat = formatTime "%Y-%m-%d"
    dayLinkFormat = formatTime "/%Y%m%d"

    anEntry EDB.Saved {..} =
           Element "h3" [] [TextNode $ EDB.title refObject]
         : S.renderBody (EDB.body refObject)
        <> [Element "p" [("class", "time")] [TextNode $ formatTime "(%H:%M)" createdAt]]

searchResult :: [EDB.Saved EDB.Entry] -> [Node]
searchResult = (row <$>)
  where
    row EDB.Saved {..} = Element "tr" []
        [ Element "td" [] [TextNode $ timeToText createdAt]
        , Element "td" [] [TextNode $ EDB.title refObject]
        , Element "td" [] [TextNode $ T.take 30 $ EDB.body refObject]
        ]

monthNavigation :: Time.Day -> H.Splice LupoHandler
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
        monthLinkFormat = T.pack . Time.formatTime L.defaultTimeLocale "/%Y%m"

    nextMonth (Time.toGregorian -> (y, 12, _)) = Time.fromGregorian (y + 1) 1 1
    nextMonth (Time.toGregorian -> (y, m, _)) = Time.fromGregorian y (m + 1) 1

    previousMonth (Time.toGregorian -> (y, 1, _)) = Time.fromGregorian (y - 1) 12 1
    previousMonth (Time.toGregorian -> (y, m, _)) = Time.fromGregorian y (m - 1) 1

dayNavigation :: Time.Day -> H.Splice LupoHandler
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
            [("href", T.pack $ Time.formatTime L.defaultTimeLocale "/%Y%m%d" d_)]
            [TextNode body]

    thisMonthLink body = Element "a"
        [("href", T.pack $ Time.formatTime L.defaultTimeLocale "/%Y%m" d)]
        [TextNode body]

    getNextDay (Time.addDays 1 -> tommorow) = do
        db <- EDB.getEntryDB
        run_ =<< (EL.head >>==) <$> EDB.afterSavedDays db tommorow

    getPreviousDay (Time.addDays (-1) -> yesterday) = do
        db <- EDB.getEntryDB
        run_ =<< (EL.head >>==) <$> EDB.beforeSavedDays db yesterday

timeToText :: Time.ZonedTime -> T.Text
timeToText = formatTime "%Y-%m-%d"

formatTime :: Time.FormatTime t => String -> t -> T.Text
formatTime fmt d = T.pack $ Time.formatTime L.defaultTimeLocale fmt d
