{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , FlexibleInstances
    , ViewPatterns
    , TemplateHaskell #-}
module Lupo.View
    ( Day(..)
    , emptyDay
    , entry
    , entryBody
    , entryInfo
    , day
    , searchResult
    , monthNavigation
    , dayNavigation
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.Syntax as S
import Lupo.Application
import Snap
import Text.XmlHtml
import qualified Data.Time.Calendar.Julian as J
import qualified Data.Time as Ti
import qualified Data.Enumerator.List as EL
import Data.Enumerator hiding (concatMap)
import qualified System.Locale as L
import qualified Text.Templating.Heist as H
import qualified Data.Text as T
import Data.Monoid

import Development.Placeholders

data Day a = Day
    { entriesDay :: Ti.Day
    , entries :: [EDB.Saved a]
    }

emptyDay :: Day a
emptyDay = Day undefined []

entry :: (Monad m, Applicative m) => EDB.Saved EDB.Entry -> H.Splice m
entry EDB.Saved {refObject = e@EDB.Entry {..}, ..} = do
    b <- entryBody e
    pure $
        [ Element "div" [("class", "entry")] $
          (Element "h2" [] [TextNode $ timeToText createdAt, TextNode " ", TextNode title]) : b ]

entryBody :: Monad m => EDB.Entry -> H.Splice m
entryBody EDB.Entry {..} = return $ S.renderBody body

entryInfo :: Monad m => [EDB.Saved EDB.Entry] -> H.Splice m
entryInfo es = return $ singleEntry <$> es
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

day :: Monad m => Day EDB.Entry -> H.Splice m
day Day {..} = return $ pure $
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

searchResult :: Monad m => [EDB.Saved EDB.Entry] -> H.Splice m
searchResult = return . (result <$>)
  where
    result EDB.Saved {..} = Element "tr" []
        [ Element "td" [] [TextNode $ timeToText createdAt]
        , Element "td" [] [TextNode $ EDB.title refObject]
        , Element "td" [] [TextNode $ T.take 30 $ EDB.body refObject]
        ]

monthNavigation :: Ti.Day -> H.Splice (Handler Lupo Lupo)
monthNavigation m = do
    pure $
        [ Element "ul" [("class", "page-navigation")]
            [ Element "li" [] [mkMonthLink "Previous Month" $ J.addJulianMonthsClip (-1) m]
            , Element "li" [] [mkMonthLink "Next Month" $ J.addJulianMonthsClip 1 m]
            ]
        ]
  where
    mkMonthLink body m_ = Element "a" [("href", monthLinkFormat m_)] [TextNode body]
      where
        monthLinkFormat = T.pack . Ti.formatTime L.defaultTimeLocale "/%Y%m"

dayNavigation :: Ti.Day -> H.Splice (Handler Lupo Lupo)
dayNavigation d = do
    next <- getNextDay d
    previous <- getPreviousDay d
    pure $
        [ Element "ul" [("class", "page-navigation")]
            [ Element "li" [] [mkDayLink "Previous Day" previous]
            , Element "li" [] [thisMonthLink]
            , Element "li" [] [mkDayLink "Next Day" next]
            ]
        ]
  where
    mkDayLink body = maybe (TextNode body) $ \d_ ->
        Element "a"
            [("href", T.pack $ Ti.formatTime L.defaultTimeLocale "/%Y%m%d" d_)]
            [TextNode body]

    thisMonthLink = Element "a"
        [("href", T.pack $ Ti.formatTime L.defaultTimeLocale "/%Y%m" d)]
        [TextNode "This Month"]

    getNextDay (Ti.addDays 1 -> tommorow) = do
        db <- EDB.getEntryDB
        run_ =<< (EL.head >>==) <$> EDB.afterSavedDays db tommorow

    getPreviousDay (Ti.addDays (-1) -> yesterday) = do
        db <- EDB.getEntryDB
        run_ =<< (EL.head >>==) <$> EDB.beforeSavedDays db yesterday
