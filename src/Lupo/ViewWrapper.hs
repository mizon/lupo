{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Lupo.ViewWrapper (
    View(..)
  , singleDayView
  , monthView
  ) where

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import Snap
import qualified Snap.Snaplet.Heist as SH
import Text.Shakespeare.Text
import qualified Text.Templating.Heist as H

import Lupo.Config
import qualified Lupo.Database as DB
import qualified Lupo.Locale as L
import qualified Lupo.Navigation as N
import Lupo.Util
import qualified Lupo.View as V

data View m = View {
    render :: m ()
  }

makeView :: SH.HasHeist b => H.Splice (Handler b b) -> View (Handler b v)
makeView ss = View {
    render = SH.heistLocal (H.bindSplice "main-body" ss) $ SH.render "public"
  }

singleDayView :: (
    m ~ H.HeistT (Handler b b)
  , GetLupoConfig m
  , L.HasLocalizer m
  , SH.HasHeist b
  ) => DB.Day -> N.Navigation m -> DB.Comment -> View (Handler b v)
singleDayView day nav c = makeView $ do
  bindBasicSplices $ formatTime "%Y-%m-%d" $ DB.day day
  H.callTemplate "day" [
      ("day-title", pure $ V.dayTitle reqDay)
    , ("entries", pure $ V.anEntry =<< DB.dayEntries day)
    , ("comments", pure $ V.comment =<< DB.dayComments day)
    , ("page-navigation", V.singleDayNavigation nav)
    , ("new-comment-url", textSplice [st|/comment/#{formatTime "%Y%m%d" reqDay}|])
    , ("comment-name", H.textSplice $ DB.commentName c)
    , ("comment-body", H.textSplice $ DB.commentBody c)
    ]
  where
    reqDay = DB.day day

monthView :: (
    m ~ H.HeistT (Handler b b)
  , L.HasLocalizer m
  , GetLupoConfig m
  , SH.HasHeist b
  ) => N.Navigation m -> [DB.Day] -> View (Handler b v)
monthView nav days = makeView $ do
  bindBasicSplices $ formatTime "%Y-%m" $ N.getThisMonth nav
  H.modifyTS $ H.bindSplices [
      ("page-navigation", V.monthNavigation nav)
    ]
  if null days then
    V.emptyMonth
  else
    pure $ V.daySummary =<< days

bindBasicSplices :: (Monad m, GetLupoConfig (H.HeistT m))
                 => T.Text -> H.HeistT m ()
bindBasicSplices title = do
  siteTitle <- refLupoConfig lcSiteTitle
  footer <- refLupoConfig lcFooterBody
  H.modifyTS $ H.bindSplices [
      ("page-title", H.textSplice $ makePageTitle siteTitle)
    , ("header-title", H.textSplice siteTitle)
    , ("style-sheet", H.textSplice "diary")
    , ("footer-body", pure footer)
    ]
  where
    makePageTitle siteTitle =
      case title of
        "" -> siteTitle
        t -> siteTitle <> " | " <> t
