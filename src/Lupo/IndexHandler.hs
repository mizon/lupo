{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.IndexHandler (
    topPageHandler
  , parseQuery
  , searchHandler
  ) where

import Control.Monad as M
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import Data.Enumerator as E hiding (head, replicate)
import qualified Data.Enumerator.List as EL
import Data.Maybe
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
  db <- LDB.getDatabase
  (zonedDay -> today) <- liftIO $ Time.getZonedTime
  latest <- run_ =<< (EL.head >>==) <$> LDB.beforeSavedDays db today
  multiDays (fromMaybe today latest) =<< refLupoConfig lcDaysPerPage

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
      reqDay <- dayParser
      pure $ withBasicViewParams (formatTime "%Y-%m-%d" reqDay) $ do
        db <- LDB.getDatabase
        day <- LDB.selectDay db reqDay
        nav <- makeNavigation reqDay
        SH.renderWithSplices "public" [
            ("main-body", pure [V.dayView day])
          , ("page-navigation", V.singleDayNavigation nav)
          ]

    monthResponse = do
      reqMonth <- monthParser
      pure $ do
        db <- LDB.getDatabase
        days_ <- run_ =<< ((toDayViews db =$ takeMonthViews reqMonth) >>==)
          <$> LDB.afterSavedDays db reqMonth
        nav <- makeNavigation reqMonth
        withBasicViewParams (formatTime "%Y-%m" reqMonth) $ SH.renderWithSplices "public" [
            ("main-body", mkBody days_)
          , ("page-navigation", V.monthNavigation nav)
          ]
      where
        mkBody [] = V.emptyMonth
        mkBody days_ = pure $ V.dayView <$> days_

        takeMonthViews m = EL.takeWhile $ isSameMonth m . LDB.day
          where
            isSameMonth (Time.toGregorian -> (year1, month1, _))
                        (Time.toGregorian -> (year2, month2, _)) =
              year1 == year2 && month1 == month2

        toDayViews db = EL.mapM (\d -> LDB.selectDay db d)

        monthParser = Time.readTime defaultTimeLocale "%Y%m" <$>
          M.sequence (replicate 6 $ A.satisfy C.isDigit)

    dayParser = Time.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)
    number = A.satisfy C.isDigit

searchHandler :: LupoHandler ()
searchHandler = do
  db <- LDB.getDatabase
  word <- param "word"
  es <- run_ =<< (EL.consume >>==) <$> LDB.search db word
  withBasicViewParams word $ SH.renderWithSplices "search-result" [
      ("main-body", pure $ V.searchResult es)
    ]

multiDays :: Time.Day -> Integer -> LupoHandler ()
multiDays from nDays = do
  db <- LDB.getDatabase
  targetDays <- run_ =<< (EL.take nDays >>==) <$> LDB.beforeSavedDays db from
  days <- Prelude.mapM (LDB.selectDay db) targetDays
  nav <- makeNavigation from
  withBasicViewParams "" $ SH.renderWithSplices "public" [
      ("main-body", pure $ V.dayView <$> days)
    , ("page-navigation", V.multiDaysNavigation nDays nav)
    ]

withBasicViewParams :: T.Text -> LupoHandler () -> LupoHandler ()
withBasicViewParams title h = do
  siteTitle <- refLupoConfig lcSiteTitle
  footer <- refLupoConfig lcFooterBody
  SH.withSplices [
      ("page-title", textSplice $ makePageTitle siteTitle)
    , ("header-title", textSplice siteTitle)
    , ("style-sheet", textSplice "diary")
    , ("footer-body", pure footer)
    ] h
  where
    makePageTitle siteTitle = case title of
      "" -> siteTitle
      t -> siteTitle <> " | " <> t

makeNavigation :: (Functor m, Applicative m, LDB.HasDatabase m, LDB.DatabaseContext n) =>
  Time.Day -> m (N.Navigation n)
makeNavigation current = N.makeNavigation <$> LDB.getDatabase <*> pure current
