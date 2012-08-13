{-# LANGUAGE OverloadedStrings
    , RecordWildCards
    , FlexibleInstances #-}
module Lupo.View
    ( entryBody
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

entryBody :: Monad m => EDB.Entry -> H.Splice m
entryBody EDB.Entry {..} = return $ S.renderBody body

entryInfo :: Monad m => [EDB.Saved EDB.Entry] -> H.Splice m
entryInfo es = return $ singleEntry <$> es
  where
    singleEntry EDB.Saved {refObject = EDB.Entry {..}, ..} =
        Element "tr" []
            [ Element "td" [("class", "date")] [TextNode timeText]
            , Element "td" [] [TextNode title]
            , Element "td" [("class", "operation")]
                [ Element "a" [("href", "/admin/" <> toText idx <> "/edit")] [TextNode "Edit"]
                , TextNode " "
                , Element "a" [ ("href", "/admin/" <> toText idx <> "/delete")
                              , ("onclick", "return confirm(\"Are you sure?\")") ] [TextNode "Delete"]
                ]
            ]
      where
        timeText = T.pack $ formatTime L.defaultTimeLocale "%Y-%m-%d" createdAt
        toText = T.pack . show
