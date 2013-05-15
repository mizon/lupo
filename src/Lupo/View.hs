{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.View
  ( View (..)
  , singleDayView
  , multiDaysView
  , monthView
  , searchResultView
  , loginView
  , initAccountView
  , adminView
  , entryEditorView
  , entryPreviewView
  , feedSplice
  ) where

import Control.Applicative
import qualified Data.List as L
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as Encoding
import Snap
import qualified Snap.Snaplet.Heist as SH
import Text.Shakespeare.Text hiding (toText)
import qualified Heist as H
import qualified Heist.Interpreted as H
import Text.XmlHtml hiding (render)

import Lupo.Application
import Lupo.Config
import qualified Lupo.Entry as E
import qualified Lupo.Locale as L
import qualified Lupo.Navigation as N
import qualified Lupo.Syntax as S
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.ViewFragment as V

data View m = View
  { render :: m ()
  }

singleDayView :: m ~ H.HeistT LupoHandler LupoHandler => E.Page -> N.Navigation m -> E.Comment -> [T.Text] -> [T.Text] -> View LupoHandler
singleDayView page nav c notice errs = makeView renderPublic $ ViewRep (formatTime "%Y-%m-%d" $ E.pageDay page) $ do
  H.callTemplate "day"
    [ ("lupo:day-title", V.dayTitle reqDay)
    , ("lupo:entries", pure entriesTemplate)
    , ("lupo:if-commented", ifCommented)
    , ("lupo:comments-caption", H.textSplice =<< L.localize "Comments")
    , ("lupo:comments", H.mapSplices V.comment $ E.pageComments page)
    , ("lupo:page-navigation", V.singleDayNavigation nav)
    , ("lupo:new-comment-caption", H.textSplice =<< L.localize "New Comment")
    , ("lupo:new-comment-notice", newCommentNotice)
    , ("lupo:new-comment-errors", newCommentErrors)
    , ("lupo:new-comment-url", U.toURLSplice =<< U.getURL U.commentPostPath <*> pure reqDay)
    , ("lupo:comment-name", H.textSplice $ E.commentName c)
    , ("lupo:comment-body", H.textSplice $ E.commentBody c)
    , ("lupo:name-label", H.textSplice =<< L.localize "Name")
    , ("lupo:content-label", H.textSplice =<< L.localize "Content")
    ]
  where
    reqDay = E.pageDay page

    entriesTemplate = concat $ snd $ L.mapAccumL accum 1 $ E.pageEntries page
      where
        accum i e = (succ i, V.anEntry (Just i) e)

    ifCommented
      | E.numOfComments page > 0 = childNodes <$> H.getParamNode
      | otherwise = pure []

    newCommentNotice
      | null notice = pure []
      | otherwise = H.callTemplate "_notice"
        [ ("lupo:notice-class", H.textSplice "notice success")
        , ("lupo:notice-messages", H.mapSplices mkRow notice)
        ]

    newCommentErrors
      | null errs = pure []
      | otherwise = H.callTemplate "_notice"
        [ ("lupo:notice-class", H.textSplice "notice error")
        , ("lupo:notice-messages", H.mapSplices mkRow errs)
        ]

    mkRow txt = do
      txt' <- L.localize txt
      pure $ [Element "li" [] [TextNode txt']]

multiDaysView :: m ~ H.HeistT LupoHandler LupoHandler => N.Navigation m -> [E.Page] -> View LupoHandler
multiDaysView nav pages = makeView renderPublic $ ViewRep title $ do
  daysPerPage <- refLupoConfig lcDaysPerPage
  H.callTemplate "multi-days"
    [ ("lupo:page-navigation", V.multiDaysNavigation daysPerPage nav)
    , ("lupo:day-summaries", H.mapSplices V.daySummary pages)
    ]
  where
    title =
      case pages of
        ds@((E.pageDay -> d) : _) -> [st|#{formatTime "%Y-%m-%d" d}-#{toText $ length ds}|]
        _ -> ""

monthView :: m ~ H.HeistT LupoHandler LupoHandler => N.Navigation m -> [E.Page] -> View LupoHandler
monthView nav pages = makeView renderPublic $ ViewRep (formatTime "%Y-%m" $ N.getThisMonth nav) $ do
  H.callTemplate "multi-days"
    [ ("lupo:page-navigation", V.monthNavigation nav)
    , ("lupo:day-summaries", if null pages then
                               V.emptyMonth
                             else
                               H.mapSplices V.daySummary pages)
    ]

searchResultView :: T.Text -> [E.Saved E.Entry] -> View LupoHandler
searchResultView word es = makeView renderPublic $ ViewRep word $ H.callTemplate "search-result"
  [ ("lupo:search-word", H.textSplice word)
  , ("lupo:search-results", H.mapSplices V.searchResult es)
  ]

loginView :: View LupoHandler
loginView = makeView renderPlain $ ViewRep "Login" $ H.callTemplate "login"
  [ ("lupo:login-url", U.toURLSplice =<< U.getURL U.loginPath)
  ]

initAccountView :: View LupoHandler
initAccountView = makeView renderPlain $ ViewRep "Init Account" $ H.callTemplate "init-account"
  [ ("lupo:init-account-url", U.toURLSplice =<< U.getURL U.initAccountPath)
  ]

adminView :: [E.Page] -> View LupoHandler
adminView days = makeView renderAdmin $ ViewRep "Lupo Admin" $ H.callTemplate "admin"
  [ ("lupo:days", H.mapSplices makeDayRow days)
  , ("lupo:new-entry-url", U.toURLSplice =<< U.getURL U.fullPath <*> pure "admin/new")
  ]
  where
    makeDayRow E.Page { E.pageEntries = []
                      } = pure []
    makeDayRow E.Page { E.pageEntries = e : es
                      , ..
                      } = do
      headRow <- (dateTh :) <$> makeEntryRow e
      tailRows <- sequence $ makeEntryRow <$> es
      pure $ tr <$> headRow : tailRows
      where
        tr t = Element "tr" [] t

        dateTh = Element "th" [("class", "date"), ("rowspan", toText $ length $ e : es)]
          [ TextNode $ formatTime "%Y-%m-%d" pageDay
          ]

        makeEntryRow e'@E.Saved {..} = do
          editPath <- Encoding.decodeUtf8 <$> (U.getURL U.entryEditPath <*> pure e')
          pure
            [ Element "td" [] [TextNode $ E.entryTitle savedContent]
            , Element "td" [("class", "action")]
              [ Element "a" [("href", editPath)] [TextNode "Edit"]
              ]
            ]

entryEditorView :: E.Saved E.Entry -> T.Text -> (U.URLMapper -> U.Path) -> View LupoHandler
entryEditorView E.Saved {..} editType editPath = makeView renderAdmin $
  ViewRep editorTitle $ H.callTemplate "entry-editor"
    [ ("lupo:editor-title", H.textSplice [st|#{editType}: #{editorTitle}: #{E.entryTitle savedContent}|])
    , ("lupo:edit-path", U.toURLSplice =<< U.getURL editPath)
    , ("lupo:entry-title", H.textSplice $ E.entryTitle savedContent)
    , ("lupo:entry-body", H.textSplice $ E.entryBody savedContent)
    ]
  where
    editorTitle = formatTime "%Y-%m-%d" createdAt

entryPreviewView :: E.Saved E.Entry -> T.Text -> (U.URLMapper -> U.Path) -> View LupoHandler
entryPreviewView e@E.Saved {..} editType editPath = makeView renderAdmin $
  ViewRep previewTitle $ H.callTemplate "entry-preview"
    [ ( "lupo:preview-title"
      , H.textSplice [st|#{editType}: #{previewTitle}: #{E.entryTitle savedContent}|]
      )
    , ("lupo:preview-body", pure $ V.anEntry Nothing e)
    , ("lupo:edit-path", U.toURLSplice =<< U.getURL editPath)
    , ("lupo:entry-title", H.textSplice $ E.entryTitle savedContent)
    , ("lupo:entry-body", H.textSplice $ E.entryBody savedContent)
    ]
  where
    previewTitle = formatTime "%Y-%m-%d" createdAt

feedSplice :: [E.Saved E.Entry] -> H.Splice LupoHandler
feedSplice entries = do
  title <- refLupoConfig lcSiteTitle
  author <- refLupoConfig lcAuthorName
  urls <- U.getURLMapper
  H.callTemplate "feed"
    [ ("lupo:feed-title", textSplice title)
    , ("lupo:last-updated", textSplice $ maybe "" (formatTime "%Y-%m-%d") lastUpdated)
    , ("lupo:index-path", textSplice $ Encoding.decodeUtf8 $ U.fullPath urls "")
    , ("lupo:feed-id", textSplice $ Encoding.decodeUtf8 $ U.fullPath urls "recent.atom")
    , ("lupo:author-name", textSplice author)
    , ("lupo:entries", H.mapSplices entryToFeed entries)
    ]
  where
    lastUpdated = E.modifiedAt <$> listToMaybe entries

entryToFeed :: E.Saved E.Entry -> H.Splice LupoHandler
entryToFeed e@E.Saved {..} = H.callTemplate "_feed-entry"
  [ ("lupo:title", textSplice $ E.entryTitle savedContent)
  , ("lupo:link", urlSplice)
  , ("lupo:entry-id", urlSplice)
  , ("lupo:published", textSplice $ formatTimeForAtom createdAt)
  , ("lupo:updated", textSplice $ formatTimeForAtom modifiedAt)
  , ("lupo:summary", textSplice $ getSummary $ E.entryBody savedContent)
  ]
  where
    getSummary = summarize . nodesToPlainText . S.renderBody
      where
        summarize t = if T.length t <= 140
                      then t
                      else T.take 140 t <> "..."

        nodesToPlainText = L.foldl' (\l r -> l <> nodeText r) ""

    urlSplice = do
      urls <- U.getURLMapper
      textSplice $ Encoding.decodeUtf8 $ U.entryPath urls e

makePageTitle :: GetLupoConfig n => ViewRep m -> n T.Text
makePageTitle ViewRep {..} = do
  siteTitle <- refLupoConfig lcSiteTitle
  pure $
    case viewTitle of
      "" -> siteTitle
      t -> [st|#{siteTitle} | #{t}|]

makeView :: (ViewRep m -> m ()) -> ViewRep m -> View m
makeView renderer rep = View $ renderer rep

data ViewRep m = ViewRep
  { viewTitle :: T.Text
  , viewSplice :: H.Splice m
  }

renderPublic :: ( h ~ H.HeistT (Handler b b) (Handler b b)
          , SH.HasHeist b
          , GetLupoConfig h
          , U.HasURLMapper h
          )
       => ViewRep (Handler b b) -> Handler b v ()
renderPublic v@ViewRep {..} = SH.heistLocal bindSplices $ SH.render "public"
  where
    bindSplices =
        H.bindSplices
        [ ("lupo:page-title", H.textSplice =<< makePageTitle v)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", U.toURLSplice =<< U.getURL U.cssPath <*> pure "diary.css")
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        , ("lupo:feed-path", U.toURLSplice =<< U.getURL U.fullPath <*> pure "recent.atom")
        , ("lupo:feed-icon-path", U.toURLSplice =<< U.getURL U.fullPath <*> pure "images/feed.png")
        ]
      . H.bindSplice "lupo:main-body" viewSplice

renderPlain :: (h ~ (Handler b b), SH.HasHeist b, U.HasURLMapper (H.HeistT h h)) => ViewRep h -> Handler b v ()
renderPlain ViewRep {..} = SH.heistLocal bindSplices $ SH.render "default"
  where
    bindSplices = H.bindSplices
      [ ("lupo:page-title", H.textSplice viewTitle)
      , ("lupo:style-sheet", U.toURLSplice =<< U.getURL U.cssPath <*> pure "plain.css")
      , ("apply-content", viewSplice)
      ]

renderAdmin :: ( h ~ H.HeistT (Handler b b) (Handler b b)
               , SH.HasHeist b
               , GetLupoConfig h
               , U.HasURLMapper h
               )
            => ViewRep (Handler b b) -> Handler b v ()
renderAdmin v@ViewRep {..} = SH.heistLocal bindSplices $ SH.render "admin-frame"
  where
    bindSplices =
        H.bindSplices
        [ ("lupo:page-title", H.textSplice =<< makePageTitle v)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", U.toURLSplice =<< U.getURL U.cssPath <*> pure "admin.css")
        , ("lupo:admin-url", U.toURLSplice =<< U.getURL U.adminPath)
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        ]
      . H.bindSplice "lupo:main-body" viewSplice
