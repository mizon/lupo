{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.ViewFragment
  ( renderBody
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

import qualified Data.List as L
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import qualified Data.Time as Time
import Snap
import Text.Shakespeare.Text hiding (toText)
import qualified Heist as H
import qualified Heist.Interpreted as H
import Text.XmlHtml

import qualified Lupo.Database as LDB
import qualified Lupo.Locale as LL
import qualified Lupo.Navigation as N
import qualified Lupo.Syntax as S
import qualified Lupo.URLMapper as U
import Lupo.Util

renderBody :: LDB.Entry -> H.Template
renderBody LDB.Entry {..} = S.renderBody entryBody

daySummary :: (Functor m, Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m))
           => LDB.Day -> H.Splice m
daySummary LDB.Day {..} = H.callTemplate "_day-summary"
  [ ("lupo:day-title", dayTitle day)
  , ("lupo:day-entries", pure $ anEntry Nothing =<< dayEntries)
  , ("lupo:link-to-comment", linkToComment)
  , ("lupo:comment-label", H.textSplice =<< commentLabel)
  ]
  where
    commentLabel
      | numOfComments > 0 = do
          label <- LL.localize "Comment"
          pure [st|#{label} (#{toText numOfComments})|]
      | otherwise = LL.localize "New Comment"

    linkToComment
      | numOfComments > 0 = U.urlSplice $ flip U.commentsPath day
      | otherwise = U.urlSplice $ flip U.newCommentPath day

dayTitle :: (U.HasURLMapper (H.HeistT m m), Monad m, Functor m) => Time.Day -> H.Splice m
dayTitle d = do
  (Encoding.decodeUtf8 -> link) <- U.getURL $ flip U.singleDayPath d
  pure $ [Element "a" [("href", link)] [TextNode $ dayFormat d]]
  where
    dayFormat = formatTime "%Y-%m-%d"

anEntry :: Maybe Int -> LDB.Saved LDB.Entry -> H.Template
anEntry index LDB.Saved {..} =
     Element "h3" entryHeadlineAttr (S.renderInline $ LDB.entryTitle savedContent)
   : S.renderBody (LDB.entryBody savedContent)
  <> [Element "p" [("class", "time")] [TextNode $ formatTime "(%H:%M)" createdAt]]
  where
    entryHeadlineAttr = maybe [] (\i -> [("id", T.justifyRight 2 '0' $ toText i)]) index

comment :: (Monad m, LL.HasLocalizer (H.HeistT m m)) => LDB.Saved LDB.Comment -> H.Splice m
comment LDB.Saved {..} = H.callTemplate "_comment"
  [ ("lupo:comment-name", H.textSplice $ LDB.commentName savedContent)
  , ("lupo:comment-time", H.textSplice $ formatTime "%Y-%m-%d %H:%M" $ createdAt)
  , ("lupo:comment-content", commentBodySplice)
  ]
  where
    commentBodySplice = pure $ L.intersperse (Element "br" [] [])
                      $ TextNode <$> (T.lines $ LDB.commentBody savedContent)

emptyMonth :: LL.HasLocalizer m => m H.Template
emptyMonth = do
  message <- LL.localize "no this month entries"
  pure [Element "p" [("class", "empty-month")] [TextNode message]]

searchResult :: (Functor m, Monad m, U.HasURLMapper (H.HeistT m m))
             => LDB.Saved LDB.Entry -> H.Splice m
searchResult e@LDB.Saved {..} = do
  (Encoding.decodeUtf8 -> dayLink) <- U.getURL $ flip U.singleDayPath $ zonedDay createdAt
  (Encoding.decodeUtf8 -> entryLink) <- U.getURL $ flip U.entryPath e
  pure
    [ Element "tr" []
      [ Element "th" [("class", "result-day")]
        [ Element "a" [("href", dayLink)] [TextNode $ timeToText createdAt]
        ]
      , Element "th" [("class", "result-title")]
        [ Element "a" [("href", entryLink)] [TextNode $ LDB.entryTitle savedContent]
        ]
      , Element "td" [] [TextNode $ T.take 30 $ LDB.entryBody savedContent]
      ]
    ]

monthNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m))
                => N.Navigation (H.HeistT m m) -> H.Splice m
monthNavigation nav = do
  previous <- N.getPreviousMonth nav
  next <- N.getNextMonth nav
  previousLabel <- LL.localize "Previous Month"
  nextLabel <- LL.localize "Next Month"
  H.callTemplate "_navigation"
    [ ("lupo:previous-link", mkMonthLink previousLabel previous)
    , ("lupo:middle-link", newestLink)
    , ("lupo:next-link", mkMonthLink nextLabel next)
    ]
  where
    mkMonthLink body = maybe (pure [TextNode body]) $ \m -> do
      (Encoding.decodeUtf8 -> monthPath) <- U.getURL $ flip U.monthPath m
      pure [Element "a" [("href", monthPath)] [TextNode body]]

singleDayNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m))
                    => N.Navigation (H.HeistT m m) -> H.Splice m
singleDayNavigation nav = do
  previous <- N.getPreviousDay nav
  next <- N.getNextDay nav
  previousLabel <- LL.localize "Previous Day"
  thisMonthLabel <- LL.localize "This Month"
  nextLabel <- LL.localize "Next Day"
  H.callTemplate "_navigation"
    [ ("lupo:previous-link", mkDayLink previousLabel previous)
    , ("lupo:middle-link", thisMonthLink thisMonthLabel)
    , ("lupo:next-link", mkDayLink nextLabel next)
    ]
  where
    mkDayLink body = maybe (pure [TextNode body]) $ \d_ -> do
      (Encoding.decodeUtf8 -> link) <- U.getURL $ flip U.singleDayPath d_
      pure [Element "a" [("href", link)] [TextNode body]]

    thisMonthLink body = do
      (Encoding.decodeUtf8 -> link) <- U.getURL $ flip U.monthPath $ N.getThisMonth nav
      pure [Element "a" [("href", link)] [TextNode body]]

multiDaysNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m))
                    => Integer -> N.Navigation (H.HeistT m m) -> H.Splice m
multiDaysNavigation nDays nav = do
  previous <- N.getPreviousPageTop nav nDays
  next <- N.getNextPageTop nav nDays
  previousLabel <- LL.localize "Previous %d Days"
  nextLabel <- LL.localize "Next %d Days"
  H.callTemplate "_navigation"
    [ ("lupo:previous-link", mkDayLink previousLabel previous)
    , ("lupo:middle-link", newestLink)
    , ("lupo:next-link", mkDayLink nextLabel next)
    ]
  where
    mkDayLink body = maybe (pure [TextNode formattedBody]) $ \d_ -> do
      (Encoding.decodeUtf8 -> link) <- U.getURL $ \urls ->
        U.multiDaysPath urls d_ $ fromIntegral nDays
      pure [Element "a" [("href", link)] [TextNode formattedBody]]
      where
        formattedBody = T.replace "%d" (toText nDays) body

timeToText :: Time.ZonedTime -> T.Text
timeToText = formatTime "%Y-%m-%d"

newestLink :: (LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m))
           => H.Splice m
newestLink = do
  label <- LL.localize "Newest"
  (Encoding.decodeUtf8 -> link) <- U.getURL U.topPagePath
  pure [Element "a" [("href", link)] [TextNode label]]
