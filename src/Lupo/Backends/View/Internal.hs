{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.Backends.View.Internal
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

import qualified Lupo.Entry as E
import Lupo.Import
import qualified Lupo.Locale as LL
import qualified Lupo.Navigation as N
import qualified Lupo.Syntax as S
import qualified Lupo.URLMapper as U
import Lupo.Util

renderBody :: E.Entry -> H.Template
renderBody e = S.renderBody $ e ^. E.entryBody

daySummary :: (Functor m, Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m)) => E.Page -> H.Splice m
daySummary p = H.callTemplate "_day-summary"
  [ ("lupo:day-title", dayTitle $ p ^. E.pageDay)
  , ("lupo:day-entries", pure $ anEntry Nothing =<< p ^. E.pageEntries)
  , ("lupo:link-to-comment", linkToComment)
  , ("lupo:comment-label", H.textSplice =<< commentLabel)
  ]
  where
    commentLabel
      | p ^. E.numOfComments > 0 = do
          label <- LL.localize "Comment"
          let num = toText $ p ^. E.numOfComments
          pure [st|#{label} (#{num})|]
      | otherwise = LL.localize "New Comment"

    linkToComment
      | p ^. E.numOfComments > 0 = U.urlSplice $ U.commentsPath $ p ^. E.pageDay
      | otherwise = U.urlSplice $ U.newCommentPath $ p ^. E.pageDay

dayTitle :: (U.HasURLMapper (H.HeistT m m), Monad m, Functor m) => Time.Day -> H.Splice m
dayTitle d = do
  link <- U.getURL $ U.singleDayPath d
  pure $ [Element "a" [("href", Encoding.decodeUtf8 link)] [TextNode $ dayFormat d]]
  where
    dayFormat = formatTime "%Y-%m-%d"

anEntry :: Maybe Int -> E.Saved E.Entry -> H.Template
anEntry i e =
     Element "h3" entryHeadlineAttr (S.renderInline $ e ^. E.savedContent . E.entryTitle)
   : S.renderBody (e ^. E.savedContent . E.entryBody)
  <> [Element "p" [("class", "time")] [TextNode $ formatTime "(%H:%M)" $ e ^. E.createdAt]]
  where
    entryHeadlineAttr = maybe [] (\i' -> [("id", T.justifyRight 2 '0' $ toText i')]) i

comment :: (Monad m, LL.HasLocalizer (H.HeistT m m)) => E.Saved E.Comment -> H.Splice m
comment e = H.callTemplate "_comment"
  [ ("lupo:comment-name", H.textSplice $ e ^. E.savedContent . E.commentName)
  , ("lupo:comment-time", H.textSplice $ formatTime "%Y-%m-%d %H:%M" $ e ^. E.createdAt)
  , ("lupo:comment-content", commentBodySplice)
  ]
  where
    commentBodySplice = pure $ L.intersperse (Element "br" [] [])
                      $ TextNode <$> (T.lines $ e ^. E.savedContent . E.commentBody)

emptyMonth :: LL.HasLocalizer m => m H.Template
emptyMonth = do
  message <- LL.localize "no this month entries"
  pure [Element "p" [("class", "empty-month")] [TextNode message]]

searchResult :: (Functor m, Monad m, U.HasURLMapper (H.HeistT m m)) => E.Saved E.Entry -> H.Splice m
searchResult e = do
  (Encoding.decodeUtf8 -> dayLink) <- U.getURL $ U.singleDayPath $ e ^. E.createdAt & zonedDay
  (Encoding.decodeUtf8 -> entryLink) <- U.getURL $ U.entryPath e
  pure
    [ Element "tr" []
      [ Element "th" [("class", "result-day")]
        [ Element "a" [("href", dayLink)] [TextNode $ e ^. E.createdAt & timeToText]
        ]
      , Element "th" [("class", "result-title")]
        [ Element "a" [("href", entryLink)] [TextNode $ e ^. E.savedContent . E.entryTitle]
        ]
      , Element "td" [] [TextNode $ T.take 30 $ e ^. E.savedContent . E.entryBody]
      ]
    ]

monthNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m)) => N.Navigation (H.HeistT m m) -> H.Splice m
monthNavigation nav = do
  previous <- nav ^! N.getPreviousMonth
  next <- nav ^! N.getNextMonth
  previousLabel <- LL.localize "Previous Month"
  nextLabel <- LL.localize "Next Month"
  H.callTemplate "_navigation"
    [ ("lupo:previous-link", mkMonthLink previousLabel previous)
    , ("lupo:middle-link", newestLink)
    , ("lupo:next-link", mkMonthLink nextLabel next)
    ]
  where
    mkMonthLink body = maybe (pure [TextNode body]) $ \m -> do
      (Encoding.decodeUtf8 -> monthPath) <- U.getURL $ U.monthPath m
      pure [Element "a" [("href", monthPath)] [TextNode body]]

singleDayNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m)) => N.Navigation (H.HeistT m m) -> H.Splice m
singleDayNavigation nav = do
  previous <- nav ^! N.getPreviousDay
  next <- nav ^! N.getNextDay
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
      link <- U.getURL $ U.singleDayPath d_
      pure [Element "a" [("href", Encoding.decodeUtf8 link)] [TextNode body]]

    thisMonthLink body = do
      link <- U.getURL $ U.monthPath $ nav ^. N.getThisMonth
      pure [Element "a" [("href", Encoding.decodeUtf8 link)] [TextNode body]]

multiDaysNavigation :: (Monad m, LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m)) => Integer -> N.Navigation (H.HeistT m m) -> H.Splice m
multiDaysNavigation nDays nav = do
  previous <- nav ^! N.getPreviousPageTop nDays
  next <- nav ^! N.getNextPageTop nDays
  previousLabel <- LL.localize "Previous %d Days"
  nextLabel <- LL.localize "Next %d Days"
  H.callTemplate "_navigation"
    [ ("lupo:previous-link", mkDayLink previousLabel previous)
    , ("lupo:middle-link", newestLink)
    , ("lupo:next-link", mkDayLink nextLabel next)
    ]
  where
    mkDayLink body = maybe (pure [TextNode formattedBody]) $ \d_ -> do
      (Encoding.decodeUtf8 -> link) <- U.getURL $ U.multiDaysPath d_ $ fromIntegral nDays
      pure [Element "a" [("href", link)] [TextNode formattedBody]]
      where
        formattedBody = T.replace "%d" (toText nDays) body

timeToText :: Time.ZonedTime -> T.Text
timeToText = formatTime "%Y-%m-%d"

newestLink :: (LL.HasLocalizer (H.HeistT m m), U.HasURLMapper (H.HeistT m m))
           => H.Splice m
newestLink = do
  label <- LL.localize "Newest"
  link <- U.getURL U.topPagePath
  pure [Element "a" [("href", Encoding.decodeUtf8 link)] [TextNode label]]
