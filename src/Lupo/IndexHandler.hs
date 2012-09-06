{-# LANGUAGE OverloadedStrings
    , ViewPatterns
    , ScopedTypeVariables #-}
module Lupo.IndexHandler
    ( top
    , parseQuery
    , search
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.View as V
import Lupo.Application
import Lupo.Util
import Lupo.Config
import qualified Text.Templating.Heist as TH
import qualified Snap.Snaplet.Heist as H
import Snap
import qualified Data.Enumerator.List as EL
import qualified Data.Attoparsec.Text as A
import qualified Data.Time as Ti
import qualified Data.Text as T
import Data.Enumerator as E hiding (head, replicate)
import qualified Data.Char as C
import Control.Monad as M
import System.Locale
import Prelude hiding (filter)

top :: Handler Lupo Lupo ()
top = do
    (getDay -> today) <- liftIO $ Ti.getZonedTime
    days today =<< refLupoConfig lcDaysPerPage
  where
    getDay = Ti.localDay . Ti.zonedTimeToLocalTime

parseQuery :: T.Text -> Handler Lupo Lupo ()
parseQuery = either (const pass) id . A.parseOnly ((A.try multi) <|> (A.try single) <|> month)
  where
    multi = do
        from <- dayParser
        void $ A.char '-'
        nentries <- read . pure <$> number
        return $ days from nentries

    single = do
        day <- dayParser
        return $ do
            db <- EDB.getEntryDB
            es <- EDB.selectDay db day
            H.renderWithSplices "public"
                [ ("page-title", textSplice "")
                , ("style-sheet", textSplice "diary")
                , ("main-body", pure . pure $ V.day $ V.Day day es)
                , ("page-navigation", H.liftHeist $ V.dayNavigation day)
                ]

    month = do
        reqMonth <- monthParser
        return $ do
            db <- EDB.getEntryDB
            days_ <- run_
                =<< ((toDayViews db =$ EL.takeWhile (isSameMonth reqMonth . V.entriesDay)) >>==)
                <$> EDB.afterSavedDays db reqMonth
            H.renderWithSplices "public"
                [ ("page-title", textSplice "")
                , ("style-sheet", textSplice "diary")
                , ("main-body", pure $ V.day <$> days_)
                , ("page-navigation", H.liftHeist $ V.monthNavigation reqMonth)
                ]
      where
        toDayViews db = EL.mapM (\d -> V.Day <$> pure d <*> EDB.selectDay db d)

        isSameMonth (Ti.toGregorian -> (year1, month1, _))
                    (Ti.toGregorian -> (year2, month2, _)) =
            year1 == year2 && month1 == month2

        monthParser = Ti.readTime defaultTimeLocale "%Y%m" <$>
            M.sequence (replicate 6 $ A.satisfy C.isDigit)

    dayParser = Ti.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)
    number = A.satisfy C.isDigit

search :: Handler Lupo Lupo ()
search = do
    db <- EDB.getEntryDB
    word <- param "word"
    es <- run_ =<< (EL.consume >>==) <$> EDB.search db word
    title <- refLupoConfig lcSiteTitle
    H.renderWithSplices "search-result"
        [ ("page-title", textSplice title)
        , ("style-sheet", textSplice "diary")
        , ("search-results", H.liftHeist $ V.searchResult es)
        ]

days :: Ti.Day -> Integer -> Handler Lupo Lupo ()
days from nDays = do
    db <- EDB.getEntryDB
    days_ <- run_ =<< (EL.take nDays >>==) <$> EDB.beforeSavedDays db from
    dayViews <- Prelude.mapM makeDayView days_
    title <- refLupoConfig lcSiteTitle
    H.renderWithSplices "index"
        [ ("page-title", textSplice title)
        , ("style-sheet", textSplice "diary")
        , ("entries", pure $ V.day <$> dayViews)
        ]
  where
    makeDayView d = EDB.getEntryDB >>= \db ->
        V.Day <$> pure d <*> EDB.selectDay db d
