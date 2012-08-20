{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , FlexibleInstances #-}
module Lupo.View
    ( Day(..)
    , emptyDay
    , entry
    , entryBody
    , entryInfo
    , day
    , searchResult
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.Syntax as S
import Text.XmlHtml
import qualified Data.Time as Ti
import qualified System.Locale as L
import qualified Text.Templating.Heist as H
import qualified Data.Text as T
import Data.Monoid
import Control.Applicative

data Day a = Day
    { entriesDay :: Ti.Day
    , entries :: [EDB.Saved a]
    }

emptyDay :: Day a
emptyDay = Day undefined []

entry :: Monad m => EDB.Saved EDB.Entry -> H.Splice m
entry EDB.Saved {refObject = e@EDB.Entry {..}, ..} = do
    b <- entryBody e
    return $
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
