{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import Prelude hiding (filter)
import Snap
import System.Locale
import Text.Shakespeare.Text hiding (toText)

import Lupo.Application
import qualified Lupo.Backends.Navigation as N
import Lupo.Config
import qualified Lupo.Entry as LE
import Lupo.Exception
import Lupo.Import
import Lupo.Navigation ()
import qualified Lupo.Notice as Notice
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.View as V

handleTop :: LupoHandler ()
handleTop = do
  mustNoPathInfo
  withEntryDB $ \(LE.EDBWrapper db) -> do
    today <- zonedDay <$> liftIO Time.getZonedTime
    latest <- run_ $ db ^. LE.beforeSavedDays today $$ EL.head
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
      from' <- dayParser
      void $ A.char '-'
      nentries <- read . pure <$> number
      pure $ renderMultiDays from' nentries

    singleDayResponse = do
      reqDay <- dayParser
      pure $ do
        withEntryDB $ \(LE.EDBWrapper db) -> do
          vf <- gets viewFactory
          day <- db ^! LE.selectPage reqDay
          let nav = N.makeNavigation db reqDay
          notice <- getNoticeDB >>= perform Notice.popAllNotice
          V.render $ V.singleDayView vf day nav (LE.Comment "" "") notice []

handleEntries :: LupoHandler ()
handleEntries = method GET $ do
  withEntryDB $ \(LE.EDBWrapper db) -> do
    i <- paramId
    entry <- db ^! LE.selectOne i
    page <- db ^! LE.selectPage (entry ^. LE.createdAt . zonedTimeToLocalTime . localDay)
    let n = maybe (error "must not happen") (+ 1)
          $ L.findIndex (== entry)
          $ page ^. LE.pageEntries
    base <- U.getURL $ U.singleDayPath $ page ^. LE.pageDay
    redirect $ TE.encodeUtf8 [st|#{TE.decodeUtf8 base}##{makeEntryNumber n}|]
  where
    makeEntryNumber = T.justifyRight 2 '0' . toText

handleSearch :: LupoHandler ()
handleSearch = do
  word <- textParam "word"
  withEntryDB $ \(LE.EDBWrapper db) -> do
    vf <- gets viewFactory
    es <- run_ $ db ^. LE.search word $$ EL.consume
    V.render $ V.searchResultView vf word es

handleComment :: LupoHandler ()
handleComment = method POST $ do
  dayStr <- textParam "day"
  withEntryDB $ \(LE.EDBWrapper db) -> do
    reqDay <- either (error . show) pure $ A.parseOnly dayParser dayStr
    comment <- LE.Comment <$> textParam "name" <*> textParam "body"
    try (db ^! LE.insertComment reqDay comment) >>= \case
      Left (InvalidField msgs) -> do
        vf <- gets viewFactory
        page <- db ^! LE.selectPage reqDay
        let nav = N.makeNavigation db reqDay
        V.render $ V.singleDayView vf page nav comment [] msgs
      Right _ -> do
        getNoticeDB >>= perform (Notice.addNotice "Your comment was posted successfully.")
        redirect =<< U.getURL (U.newCommentPath reqDay)

handleFeed :: LupoHandler ()
handleFeed = method GET $ withEntryDB $ \(LE.EDBWrapper db) -> do
  vf <- gets viewFactory
  entries <- E.run_ $ db ^. LE.selectAll $$ EL.take 10
  V.render $ V.entriesFeed vf entries

monthResponse :: A.Parser (LupoHandler ())
monthResponse = do
  reqMonth <- monthParser
  pure $ withEntryDB $ \(LE.EDBWrapper db) -> do
    vf <- gets viewFactory
    let nav = N.makeNavigation db reqMonth
    days <- run_ $ db ^. LE.afterSavedDays reqMonth
                $$ toDayContents db
                =$ takeSameMonthDays reqMonth
    V.render $ V.monthView vf nav days
  where
    takeSameMonthDays m = EL.takeWhile $ isSameMonth m . view LE.pageDay
      where
        isSameMonth (Time.toGregorian -> (year1, month1, _))
                    (Time.toGregorian -> (year2, month2, _)) =
          year1 == year2 && month1 == month2

    toDayContents db = EL.mapM $ \d ->
      db ^! LE.selectPage d

    monthParser = Time.readTime defaultTimeLocale "%Y%m"
              <$> M.sequence (replicate 6 $ A.satisfy C.isDigit)

renderMultiDays :: Time.Day -> Integer -> LupoHandler ()
renderMultiDays from' nDays = withEntryDB $ \(LE.EDBWrapper db) -> do
  vf <- gets viewFactory
  let nav = N.makeNavigation db from'
  targetDays <- run_ $ db ^. LE.beforeSavedDays from' $$ EL.take nDays
  pages <- Prelude.mapM (\d -> db ^! LE.selectPage d) targetDays
  V.render $ V.multiDaysView vf nav pages

dayParser :: A.Parser Time.Day
dayParser = Time.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)

number :: A.Parser Char
number = A.satisfy C.isDigit
