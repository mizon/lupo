{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.IndexHandler
  ( topPageHandler
  , parseQuery
  , searchHandler
  ) where

import Control.Monad as M
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import Data.Enumerator as E hiding (head, replicate)
import qualified Data.Enumerator.List as EL
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Time as Time
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Heist as SH
import System.Locale

import Lupo.Application
import Lupo.Config
import qualified Lupo.Database as LDB
import qualified Lupo.Navigation as N
import Lupo.Util
import qualified Lupo.View as V

topPageHandler :: LupoHandler ()
topPageHandler = do
  (getDay -> today) <- liftIO $ Time.getZonedTime
  multiDays today =<< refLupoConfig lcDaysPerPage
  where
    getDay = Time.localDay . Time.zonedTimeToLocalTime

parseQuery :: T.Text -> LupoHandler ()
parseQuery = parseQuery' $
      A.try multiDaysResponse
  <|> A.try singleDayResponse
  <|> monthResponse
  where
    parseQuery' parser = either (const pass) id . A.parseOnly parser

    multiDaysResponse = do
      from <- dayParser
      void $ A.char '-'
      nentries <- read . pure <$> number
      pure $ multiDays from nentries

    singleDayResponse = do
      day <- dayParser
      pure $ withBasicViewParams (formatTime "%Y-%m-%d" day) $ do
        db <- LDB.getDatabase
        es <- LDB.selectDay db day
        nav <- makeNavigation day
        SH.renderWithSplices "public"
          [ ("main-body", pure [V.dayView $ V.DayView day es])
          , ("page-navigation", V.singleDayNavigation =<< nav)
          ]

    monthResponse = do
      reqMonth <- monthParser
      pure $ do
        db <- LDB.getDatabase
        days_ <- run_ =<< ((toDayViews db =$ takeMonthViews reqMonth) >>==)
          <$> LDB.afterSavedDays db reqMonth
        nav <- makeNavigation reqMonth
        withBasicViewParams (formatTime "%Y-%m" reqMonth) $ SH.renderWithSplices "public"
          [ ("main-body", mkBody days_)
          , ("page-navigation", V.monthNavigation =<< nav)
          ]
      where
        mkBody [] = V.emptyMonth
        mkBody days_ = pure $ V.dayView <$> days_

        takeMonthViews m = EL.takeWhile $ isSameMonth m . V.entriesDay
          where
            isSameMonth (Time.toGregorian -> (year1, month1, _))
                        (Time.toGregorian -> (year2, month2, _)) =
              year1 == year2 && month1 == month2

        toDayViews db = EL.mapM (\d -> V.DayView <$> pure d <*> LDB.selectDay db d)

        monthParser = Time.readTime defaultTimeLocale "%Y%m" <$>
          M.sequence (replicate 6 $ A.satisfy C.isDigit)

    dayParser = Time.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)
    number = A.satisfy C.isDigit

searchHandler :: LupoHandler ()
searchHandler = do
  db <- LDB.getDatabase
  word <- param "word"
  es <- run_ =<< (EL.consume >>==) <$> LDB.search db word
  withBasicViewParams word $ SH.renderWithSplices "search-result"
    [ ("main-body", pure $ V.searchResult es)
    ]

multiDays :: Time.Day -> Integer -> LupoHandler ()
multiDays from nDays = do
  db <- LDB.getDatabase
  days_ <- run_ =<< (EL.take nDays >>==) <$> LDB.beforeSavedDays db from
  dayViews <- Prelude.mapM makeDayView days_
  nav <- makeNavigation from
  withBasicViewParams "" $ SH.renderWithSplices "public"
    [ ("main-body", pure $ V.dayView <$> dayViews)
    , ("page-navigation", V.multiDaysNavigation nDays =<< nav)
    ]
  where
    makeDayView d = do
      db <- LDB.getDatabase
      V.DayView <$> pure d <*> LDB.selectDay db d

withBasicViewParams :: T.Text -> LupoHandler () -> LupoHandler ()
withBasicViewParams title h = do
  siteTitle <- refLupoConfig lcSiteTitle
  footer <- refLupoConfig lcFooterBody
  SH.withSplices
    [ ("page-title", textSplice $ makePageTitle siteTitle)
    , ("header-title", textSplice siteTitle)
    , ("style-sheet", textSplice "diary")
    , ("footer-body", pure footer)
    ] h
  where
    makePageTitle siteTitle = case title of
      "" -> siteTitle
      t -> siteTitle <> " | " <> t

makeNavigation ::
  ( Applicative m
  , Functor n, Applicative n, Monad n, LDB.HasDatabase n
  ) => Time.Day -> m (n (N.Navigation n))
makeNavigation current = pure $ N.makeNavigation <$> LDB.getDatabase <*> pure current
