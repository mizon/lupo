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
  , singleDayView
  , multiDaysView
  , monthView
  , searchResultView
  , loginView
  , initAccount
  ) where

import Control.Applicative
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
import Lupo.Util
import qualified Lupo.ViewFragment as V

data View m = View {
    viewTitle :: T.Text
  , viewSplice :: H.Splice m
  }

render :: (SH.HasHeist b, GetLupoConfig (H.HeistT (Handler b b)))
       => View (Handler b b) -> Handler b v ()
render View {..} = SH.heistLocal bindSplices $ SH.render "public"
  where
    bindSplices =
        H.bindSplices [
          ("lupo:page-title", H.textSplice =<< makePageTitle)
        , ("lupo:site-title", H.textSplice =<< refLupoConfig lcSiteTitle)
        , ("lupo:style-sheet", H.textSplice "diary")
        , ("lupo:footer-body", refLupoConfig lcFooterBody)
        ]
      . H.bindSplice "lupo:main-body" viewSplice
      where
        makePageTitle = do
          siteTitle <- refLupoConfig lcSiteTitle
          pure $
            case viewTitle of
              "" -> siteTitle
              t -> [st|#{siteTitle} | #{t}|]

renderPlain :: SH.HasHeist b => View (Handler b b) -> Handler b v ()
renderPlain View {..} = SH.heistLocal bindSplices $ SH.render "default"
  where
    bindSplices = H.bindSplices [
        ("lupo:page-title", H.textSplice viewTitle)
      , ("lupo:style-sheet", H.textSplice "plain")
      , ("content", viewSplice)
      ]

singleDayView :: (m ~ H.HeistT n, Monad n, GetLupoConfig m, L.HasLocalizer m)
              => DB.Day -> N.Navigation m -> DB.Comment -> [T.Text] -> View n
singleDayView day nav c errs = View (formatTime "%Y-%m-%d" $ DB.day day) $ do
  H.callTemplate "day" [
      ("lupo:day-title", pure $ V.dayTitle reqDay)
    , ("lupo:entries", pure $ V.anEntry =<< DB.dayEntries day)
    , ("lupo:if-commented", ifCommented)
    , ("lupo:comments-caption", H.textSplice =<< L.localize "Comments")
    , ("lupo:comments", H.mapSplices V.comment $ DB.dayComments day)
    , ("lupo:page-navigation", V.singleDayNavigation nav)
    , ("lupo:new-comment-caption", H.textSplice =<< L.localize "New Comment")
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

    ifCommented
      | DB.numOfComments day > 0 = childNodes <$> H.getParamNode
      | otherwise = pure []

    newCommentErrors
      | null errs = pure []
      | otherwise = H.callTemplate "_notice" [
          ("lupo:notice-class", H.textSplice "notice error")
        , ("lupo:notice-messages", H.mapSplices mkList errs)
        ]
      where
        mkList txt = do
          txt' <- L.localize txt
          pure $ [Element "li" [] [TextNode txt']]

multiDaysView :: (m ~ H.HeistT n, Functor n, Monad n, L.HasLocalizer m, GetLupoConfig m)
              => N.Navigation m -> [DB.Day] -> View n
multiDaysView nav days = View [st|#{formatTime "%Y-%m-%d" firstDay}-#{toText numOfDays}|] $ do
  daysPerPage <- refLupoConfig lcDaysPerPage
  H.callTemplate "multi-days" [
      ("lupo:page-navigation", V.multiDaysNavigation daysPerPage nav)
    , ("lupo:day-summaries", H.mapSplices V.daySummary days)
    ]
  where
    firstDay = DB.day $ head days
    numOfDays = length days

monthView :: (m ~ H.HeistT n, Functor n, Monad n, L.HasLocalizer m, GetLupoConfig m)
          => N.Navigation m -> [DB.Day] -> View n
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

initAccount :: Monad m => View m
initAccount = View "Init Account" $ H.callTemplate "init-account" []
