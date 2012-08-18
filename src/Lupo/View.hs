{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , FlexibleInstances #-}
module Lupo.View
    ( entry
    , entryBody
    , entryInfo
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.Syntax as S
import Text.XmlHtml
import Data.Time
import qualified System.Locale as L
import qualified Text.Templating.Heist as H
import qualified Data.Text as T
import Data.Monoid
import Control.Applicative

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

timeToText :: ZonedTime -> T.Text
timeToText = T.pack . formatTime L.defaultTimeLocale "%Y-%m-%d"
