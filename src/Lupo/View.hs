{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.View (
    View(..)
  , render
  , singleDayView
  , multiDaysView
  , monthView
  , searchResultView
  ) where

import Control.Applicative
import Data.Monoid
import qualified Data.Text as T
import Snap
import qualified Snap.Snaplet.Heist as SH
import Text.Shakespeare.Text hiding (toText)
import qualified Text.Templating.Heist as H

import Lupo.Config
import qualified Lupo.Database as DB
import qualified Lupo.Locale as L
import qualified Lupo.Navigation as N
import Lupo.Util
import qualified Lupo.ViewFragment as V

newtype View m = View {
    getSplice :: H.Splice m
  }

render :: SH.HasHeist b => View (Handler b b) -> Handler b v ()
render (getSplice -> s) = SH.heistLocal (H.bindSplice "main-body" s) $ SH.render "public"

singleDayView :: (m ~ H.HeistT n, Monad n, GetLupoConfig m, L.HasLocalizer m)
              => DB.Day -> N.Navigation m -> DB.Comment -> View n
singleDayView day nav c = View $ do
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

multiDaysView :: (m ~ H.HeistT n, Functor n, MonadPlus n, L.HasLocalizer m, GetLupoConfig m)
              => N.Navigation m -> [DB.Day] -> View n
multiDaysView nav days = View $ do
  daysPerPage <- refLupoConfig lcDaysPerPage
  bindBasicSplices [st|#{formatTime "%Y-%m-%d" firstDay}-#{toText numOfDays}|]
  H.modifyTS $ H.bindSplices [
      ("page-navigation", V.multiDaysNavigation daysPerPage nav)
    ]
  ds <- sequence $ V.daySummary <$> days
  pure $ concat ds
  where
    firstDay = DB.day $ head days
    numOfDays = length days

monthView :: (m ~ H.HeistT n, Functor n, Monad n, L.HasLocalizer m, GetLupoConfig m)
          => N.Navigation m -> [DB.Day] -> View n
monthView nav days = View $ do
  bindBasicSplices $ formatTime "%Y-%m" $ N.getThisMonth nav
  H.modifyTS $ H.bindSplices [
      ("page-navigation", V.monthNavigation nav)
    ]
  if null days then
    V.emptyMonth
  else do
    ds <- sequence $ V.daySummary <$> days
    pure $ concat ds

searchResultView :: (Monad m, GetLupoConfig (H.HeistT m))
                 => T.Text -> [DB.Saved DB.Entry] -> View m
searchResultView word es = View $ do
  bindBasicSplices word
  void $ H.callTemplate "search-result" []
  pure $ V.searchResult es

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
