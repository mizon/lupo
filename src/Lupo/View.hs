{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.View (
    View(..)
  , render
  , renderPlain
  , renderAdmin
  , singleDayView
  , multiDaysView
  , monthView
  , searchResultView
  , loginView
  , initAccountView
  , adminView
  , entryEditorView
  ) where

import Control.Applicative
import qualified Data.List as L
import qualified Data.Text as T
import Snap
import qualified Snap.Snaplet.Heist as SH
import Text.Shakespeare.Text hiding (toText)
import qualified Text.Templating.Heist as H
import Text.XmlHtml hiding (render)

import Lupo.Config
import qualified Lupo.Database as DB
import qualified Lupo.Locale as L
import qualified Lupo.Navigation as N
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.ViewFragment as V

data View m = View {
    viewTitle :: T.Text
  , viewSplice :: H.Splice m
  }

render :: (SH.HasHeist b, GetLupoConfig (H.HeistT (Handler b b)))
       => View (Handler b b) -> Handler b v ()
render v@View {..} = SH.heistLocal bindSplices $ SH.render "public"
  where
    bindSplices =
        H.bindSplices [
          ("lupo:page-title", H.textSplice =<< makePageTitle v)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", H.textSplice "diary")
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        ]
      . H.bindSplice "lupo:main-body" viewSplice

renderPlain :: SH.HasHeist b => View (Handler b b) -> Handler b v ()
renderPlain View {..} = SH.heistLocal bindSplices $ SH.render "default"
  where
    bindSplices = H.bindSplices [
        ("lupo:page-title", H.textSplice viewTitle)
      , ("lupo:style-sheet", H.textSplice "plain")
      , ("content", viewSplice)
      ]

renderAdmin :: (SH.HasHeist b, GetLupoConfig (H.HeistT (Handler b b)))
            => View (Handler b b) -> Handler b v ()
renderAdmin v@View {..} = SH.heistLocal bindSplices $ SH.render "admin-frame"
  where
    bindSplices =
        H.bindSplices [
          ("lupo:page-title", H.textSplice =<< makePageTitle v)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", H.textSplice "admin")
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        ]
      . H.bindSplice "lupo:main-body" viewSplice

singleDayView :: (
    m ~ H.HeistT n
  , Functor n, Monad n
  , GetLupoConfig m
  , L.HasLocalizer m
  , U.HasURLMapper m
  ) => DB.Day -> N.Navigation m -> DB.Comment -> [T.Text] -> [T.Text] -> View n
singleDayView day nav c notice errs = View (formatTime "%Y-%m-%d" $ DB.day day) $ do
  H.callTemplate "day" [
      ("lupo:day-title", V.dayTitle reqDay)
    , ("lupo:entries", pure entriesTemplate)
    , ("lupo:if-commented", ifCommented)
    , ("lupo:comments-caption", H.textSplice =<< L.localize "Comments")
    , ("lupo:comments", H.mapSplices V.comment $ DB.dayComments day)
    , ("lupo:page-navigation", V.singleDayNavigation nav)
    , ("lupo:new-comment-caption", H.textSplice =<< L.localize "New Comment")
    , ("lupo:new-comment-notice", newCommentNotice)
    , ("lupo:new-comment-errors", newCommentErrors)
    , ("lupo:new-comment-url",
       textSplice [st|/#{formatTime "%Y%m%d" reqDay}/comment#new-comment|])
    , ("lupo:comment-name", H.textSplice $ DB.commentName c)
    , ("lupo:comment-body", H.textSplice $ DB.commentBody c)
    , ("lupo:name-label", H.textSplice =<< L.localize "Name")
    , ("lupo:content-label", H.textSplice =<< L.localize "Content")
    ]
  where
    reqDay = DB.day day

    entriesTemplate = concat $ snd $ L.mapAccumL accum 1 $ DB.dayEntries day
      where
        accum i e = (succ i, V.anEntry (Just i) e)

    ifCommented
      | DB.numOfComments day > 0 = childNodes <$> H.getParamNode
      | otherwise = pure []

    newCommentNotice
      | null notice = pure []
      | otherwise = H.callTemplate "_notice" [
          ("lupo:notice-class", H.textSplice "notice success")
        , ("lupo:notice-messages", H.mapSplices mkRow notice)
        ]

    newCommentErrors
      | null errs = pure []
      | otherwise = H.callTemplate "_notice" [
          ("lupo:notice-class", H.textSplice "notice error")
        , ("lupo:notice-messages", H.mapSplices mkRow errs)
        ]

    mkRow txt = do
      txt' <- L.localize txt
      pure $ [Element "li" [] [TextNode txt']]

multiDaysView :: (
    m ~ H.HeistT n
  , Functor n
  , Monad n
  , L.HasLocalizer m
  , GetLupoConfig m
  , U.HasURLMapper m
  ) => N.Navigation m -> [DB.Day] -> View n
multiDaysView nav days = View title $ do
  daysPerPage <- refLupoConfig lcDaysPerPage
  H.callTemplate "multi-days" [
      ("lupo:page-navigation", V.multiDaysNavigation daysPerPage nav)
    , ("lupo:day-summaries", H.mapSplices V.daySummary days)
    ]
  where
    title =
      case days of
        ds@((DB.day -> d) : _) -> [st|#{formatTime "%Y-%m-%d" d}-#{toText $ length ds}|]
        _ -> ""

monthView :: (
    m ~ H.HeistT n
  , Functor n
  , Monad n
  , L.HasLocalizer m
  , GetLupoConfig m
  , U.HasURLMapper m
  ) => N.Navigation m -> [DB.Day] -> View n
monthView nav days = View (formatTime "%Y-%m" $ N.getThisMonth nav) $ do
  H.callTemplate "multi-days" [
      ("lupo:page-navigation", V.monthNavigation nav)
    , ("lupo:day-summaries", if null days then
                               V.emptyMonth
                             else
                               H.mapSplices V.daySummary days)
    ]

searchResultView :: (MonadIO m, GetLupoConfig (H.HeistT m))
                 => T.Text -> [DB.Saved DB.Entry] -> View m
searchResultView word es = View word $
  H.callTemplate "search-result" [
    ("lupo:search-word", H.textSplice word)
  , ("lupo:search-results", pure $ V.searchResult <$> es)
  ]

loginView :: Monad m => View m
loginView = View "Login" $ H.callTemplate "login" []

initAccountView :: Monad m => View m
initAccountView = View "Init Account" $ H.callTemplate "init-account" []

adminView :: (Functor m, Monad m) => [DB.Day] -> View m
adminView days = View "Lupo Admin" $ H.callTemplate "admin" [
    ("lupo:days", pure $ makeDayRow =<< days)
  ]
  where
    makeDayRow DB.Day {
        DB.dayEntries = []
      } = []
    makeDayRow DB.Day {
        DB.dayEntries = e : es
      , ..
      } = tr =<< (dateTh : makeEntryRow e) : (makeEntryRow <$> es)
      where
        tr t = [Element "tr" [] t]

        dateTh = Element "th" [("class", "date"), ("rowspan", toText $ length $ e : es)] [
            TextNode $ formatTime "%Y-%m-%d" day
          ]

        makeEntryRow DB.Saved {..} = [
            Element "td" [] [TextNode $ DB.entryTitle savedContent]
          , Element "td" [("class", "action")] [
              Element "a" [("href", [st|/admin/#{toText idx}/edit|])] [TextNode "Edit"]
            ]
          ]

entryEditorView :: (Monad m, L.HasLocalizer (H.HeistT m))
                => DB.Saved DB.Entry -> T.Text -> T.Text -> View m
entryEditorView DB.Saved {..} editType editPath =
  View editorTitle $ H.callTemplateWithText "entry-editor" [
    ("lupo:editor-title", [st|#{editType}: #{editorTitle}: #{DB.entryTitle savedContent}|])
  , ("lupo:edit-path", editPath)
  , ("lupo:entry-title", DB.entryTitle savedContent)
  , ("lupo:entry-body", DB.entryBody savedContent)
  ]
  where
    editorTitle = formatTime "%Y-%m-%d" createdAt

makePageTitle :: GetLupoConfig n => View m -> n T.Text
makePageTitle View {..} = do
  siteTitle <- refLupoConfig lcSiteTitle
  pure $
    case viewTitle of
      "" -> siteTitle
      t -> [st|#{siteTitle} | #{t}|]
