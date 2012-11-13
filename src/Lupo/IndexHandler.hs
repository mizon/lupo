{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.IndexHandler (
    handleTop
  , handleEntries
  , handleSearch
  , handleComment
  ) where

import Control.Monad as M
import qualified Data.Attoparsec.Text as A
import qualified Data.Char as C
import Data.Enumerator as E hiding (head, replicate)
import qualified Data.Enumerator.List as EL
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Heist as SH
import Text.Shakespeare.Text
import System.Locale

import Lupo.Application
import Lupo.Config
import qualified Lupo.Database as LDB
import qualified Lupo.Navigation as N
import Lupo.Util
import qualified Lupo.View as V

handleTop :: LupoHandler ()
handleTop = do
  db <- LDB.getDatabase
  (zonedDay -> today) <- liftIO $ Time.getZonedTime
  latest <- run_ =<< (EL.head >>==) <$> LDB.beforeSavedDays db today
  renderMultiDays (fromMaybe today latest) =<< refLupoConfig lcDaysPerPage

handleEntries :: T.Text -> LupoHandler ()
handleEntries = parseQuery $
      A.try multiDaysResponse
  <|> A.try singleDayResponse
  <|> monthResponse
  where
    parseQuery parser = either (const pass) id . A.parseOnly parser

    multiDaysResponse = do
      from <- dayParser
      void $ A.char '-'
      nentries <- read . pure <$> number
      pure $ renderMultiDays from nentries

    singleDayResponse = do
      reqDay <- dayParser
      pure $ withBasicViewParams (formatTime "%Y-%m-%d" reqDay) $ do
        db <- LDB.getDatabase
        day <- LDB.selectDay db reqDay
        nav <- makeNavigation reqDay
        SH.renderWithSplices "day" [
            ("day-title", pure $ V.dayTitle reqDay)
          , ("entries", pure $ V.anEntry =<< LDB.dayEntries day)
          , ("comments", pure $ V.comment =<< LDB.dayComments day)
          , ("page-navigation", V.singleDayNavigation nav)
          , ("new-comment-url", textSplice [st|/comment/#{formatTime "%Y%m%d" reqDay}|])
          , ("comment-name", textSplice "")
          , ("comment-body", textSplice "")
          ]

handleSearch :: LupoHandler ()
handleSearch = do
  db <- LDB.getDatabase
  word <- param "word"
  enum <- LDB.search db word
  es <- run_ $ enum $$ EL.consume
  withBasicViewParams word $ SH.renderWithSplices "search-result" [
      ("main-body", pure $ V.searchResult es)
    ]

handleComment :: LupoHandler ()
handleComment = do
  dayStr <- param "day"
  day <- either (error . show) pure $ A.parseOnly dayParser dayStr
  comment <- LDB.Comment <$> param "name" <*> param "body"
  db <- LDB.getDatabase
  LDB.insertComment db day comment
  redirect $ "/" <> TE.encodeUtf8 dayStr

monthResponse :: A.Parser (LupoHandler ())
monthResponse = do
  reqMonth <- monthParser
  pure $ do
    db <- LDB.getDatabase
    enum <- LDB.afterSavedDays db reqMonth
    days <- run_ $ enum $$ toDayViews db =$ takeMonthViews reqMonth
    nav <- makeNavigation reqMonth
    withBasicViewParams (formatTime "%Y-%m" reqMonth) $ SH.renderWithSplices "public" [
        ("main-body", mkBody days)
      , ("page-navigation", V.monthNavigation nav)
      ]
  where
    mkBody [] = V.emptyMonth
    mkBody days_ = pure $ V.daySummary =<< days_

    takeMonthViews m = EL.takeWhile $ isSameMonth m . LDB.day
      where
        isSameMonth (Time.toGregorian -> (year1, month1, _))
                    (Time.toGregorian -> (year2, month2, _)) =
          year1 == year2 && month1 == month2

    toDayViews db = EL.mapM $ LDB.selectDay db

    monthParser = Time.readTime defaultTimeLocale "%Y%m" <$>
      M.sequence (replicate 6 $ A.satisfy C.isDigit)

renderMultiDays :: Time.Day -> Integer -> LupoHandler ()
renderMultiDays from nDays = do
  db <- LDB.getDatabase
  enum <- LDB.beforeSavedDays db from
  targetDays <- run_ $ enum $$ EL.take nDays
  days <- Prelude.mapM (LDB.selectDay db) targetDays
  nav <- makeNavigation from
  withBasicViewParams "" $ SH.renderWithSplices "public" [
      ("main-body", pure $ V.daySummary =<< days)
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
    makePageTitle siteTitle =
      case title of
        "" -> siteTitle
        t -> siteTitle <> " | " <> t

makeNavigation :: (Functor m, Applicative m, LDB.HasDatabase m, LDB.DatabaseContext n) =>
  Time.Day -> m (N.Navigation n)
makeNavigation current = N.makeNavigation <$> LDB.getDatabase <*> pure current

dayParser :: A.Parser Time.Day
dayParser = Time.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)

number :: A.Parser Char
number = A.satisfy C.isDigit
