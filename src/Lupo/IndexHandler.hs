{-# LANGUAGE OverloadedStrings
    , ViewPatterns
    , ScopedTypeVariables #-}
module Lupo.IndexHandler
    ( top
    , parseQuery
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
import Data.Maybe
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
parseQuery = either (const pass) id . A.parseOnly ((A.try multi) <|> single)
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
            H.renderWithSplices "index"
                [ ("page-title", textSplice "")
                , ("style-sheet", textSplice "diary")
                , ("entries", H.liftHeist $ V.day $ V.Day day es)
                ]

    dayParser = Ti.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)
    number = A.satisfy C.isDigit

days :: Ti.Day -> Integer -> Handler Lupo Lupo ()
days from nDays = do
    db <- EDB.getEntryDB
    days_ <- run_ =<< ($$ EL.take nDays) <$> EDB.beforeSavedDays db from
    dayViews <- Prelude.mapM makeDayView $ from : days_
    title <- refLupoConfig lcSiteTitle
    H.renderWithSplices "index"
        [ ("page-title", textSplice title)
        , ("style-sheet", textSplice "diary")
        , ("entries", H.liftHeist $ TH.mapSplices V.day dayViews)
        ]
  where
    makeDayView d = EDB.getEntryDB >>= \db ->
        V.Day <$> pure d <*> EDB.selectDay db d

nextDay :: EDB.MonadEntryDB m => Ti.Day -> m (Maybe Ti.Day)
nextDay d = EDB.getEntryDB >>= \db ->
    run_ =<< ($$ EL.head) <$> EDB.afterSavedDays db d

previousDay :: EDB.MonadEntryDB m => Ti.Day -> m (Maybe Ti.Day)
previousDay d = EDB.getEntryDB >>= \db ->
    run_ =<< ($$ EL.head) <$> EDB.beforeSavedDays db d
