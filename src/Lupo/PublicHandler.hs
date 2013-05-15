{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.PublicHandler
  ( handleTop
  , handleDay
  , handleEntries
  , handleSearch
  , handleComment
  , handleFeed
  ) where

import Control.Lens.Getter
import Control.Monad as M
import Control.Monad.CatchIO
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString as BS
import qualified Data.Char as C
import Data.Enumerator as E hiding (head, replicate)
import qualified Data.Enumerator.List as EL
import qualified Data.List as L
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Time as Time
import Prelude hiding (catch, filter)
import Snap
import System.Locale
import Text.Shakespeare.Text hiding (toText)

import Lupo.Application
import qualified Lupo.Backend.Navigation as N
import Lupo.Config
import qualified Lupo.Entry as LE
import Lupo.Exception
import qualified Lupo.Navigation as N
import qualified Lupo.Notice as Notice
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.View as V

handleTop :: LupoHandler ()
handleTop = do
  mustNoPathInfo
  db <- LE.getDatabase
  today <- zonedDay <$> liftIO Time.getZonedTime
  latest <- run_ $ LE.beforeSavedDays db today $$ EL.head
  renderMultiDays (fromMaybe today latest) =<< refLupoConfig lcDaysPerPage
  where
    mustNoPathInfo = do
      path' <- rqPathInfo <$> getRequest
      unless (BS.null path') pass

handleDay :: T.Text -> LupoHandler ()
handleDay = parseQuery $
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
      pure $ do
        db <- LE.getDatabase
        day <- LE.selectPage db reqDay
        nav <- makeNavigation reqDay
        notice <- Notice.popAllNotice =<< getNoticeDB
        V.render $ V.singleDayView day nav (LE.Comment "" "") notice []

handleEntries :: LupoHandler ()
handleEntries = method GET $ do
  db <- LE.getDatabase
  entry <- join $ LE.selectOne <$> pure db <*> paramId
  page <- LE.selectPage db $ LE.createdAt entry ^. zonedTimeToLocalTime ^. localDay
  let n = maybe (error "must not happen") (+ 1)
        $ L.findIndex (== entry)
        $ LE.pageEntries page
  base <- U.getURL U.singleDayPath <*> pure (LE.pageDay page)
  redirect $ TE.encodeUtf8 [st|#{TE.decodeUtf8 base}##{makeEntryNumber n}|]
  where
    makeEntryNumber = T.justifyRight 2 '0' . toText

handleSearch :: LupoHandler ()
handleSearch = do
  word <- textParam "word"
  esE <- LE.search <$> LE.getDatabase <*> pure word
  es <- run_ $ esE $$ EL.consume
  V.render $ V.searchResultView word es

handleComment :: LupoHandler ()
handleComment = method POST $ do
  dayStr <- textParam "day"
  db <- LE.getDatabase
  reqDay <- either (error . show) pure $ A.parseOnly dayParser dayStr
  comment <- LE.Comment <$> textParam "name" <*> textParam "body"
  cond <- try $ LE.insertComment db reqDay comment
  case cond of
    Left (InvalidField msgs) -> do
      page <- LE.selectPage db reqDay
      nav <- makeNavigation reqDay
      V.render $ V.singleDayView page nav comment [] msgs
    Right _ -> do
      ndb <- getNoticeDB
      Notice.addNotice ndb "Your comment was posted successfully."
      redirect =<< U.getURL U.newCommentPath <*> pure reqDay

handleFeed :: LupoHandler ()
handleFeed = method GET $ do
  db <- LE.getDatabase
  entries <- E.run_ $ LE.selectAll db $$ EL.take 10
  V.render $ V.entriesFeed entries

monthResponse :: A.Parser (LupoHandler ())
monthResponse = do
  reqMonth <- monthParser
  pure $ do
    db <- LE.getDatabase
    nav <- makeNavigation reqMonth
    days <- run_ $ LE.afterSavedDays db reqMonth $$ toDayContents db =$ takeSameMonthDays reqMonth
    V.render $ V.monthView nav days
  where
    takeSameMonthDays m = EL.takeWhile $ isSameMonth m . LE.pageDay
      where
        isSameMonth (Time.toGregorian -> (year1, month1, _))
                    (Time.toGregorian -> (year2, month2, _)) =
          year1 == year2 && month1 == month2

    toDayContents db = EL.mapM $ LE.selectPage db

    monthParser = Time.readTime defaultTimeLocale "%Y%m"
              <$> M.sequence (replicate 6 $ A.satisfy C.isDigit)

renderMultiDays :: Time.Day -> Integer -> LupoHandler ()
renderMultiDays from nDays = do
  db <- LE.getDatabase
  targetDays <- run_ $ LE.beforeSavedDays db from $$ EL.take nDays
  nav <- makeNavigation from
  pages <- Prelude.mapM (LE.selectPage db) targetDays
  V.render $ V.multiDaysView nav pages

makeNavigation :: (Functor m, Applicative m, LE.HasDatabase m, LE.DatabaseContext n) => Time.Day -> m (N.Navigation n)
makeNavigation current = N.makeNavigation <$> LE.getDatabase <*> pure current

dayParser :: A.Parser Time.Day
dayParser = Time.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)

number :: A.Parser Char
number = A.satisfy C.isDigit
