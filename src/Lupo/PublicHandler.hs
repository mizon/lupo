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
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Time as Time
import Prelude hiding (filter)
import Snap
import System.Locale

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
  today <- zonedDay <$> liftIO Time.getZonedTime
  latest <- withEntryDB $ \(LE.EDBWrapper db) ->
    run_ $ db ^. LE.beforeSavedDays today $$ EL.head
  renderMultiDays (fromMaybe today latest) =<< refLupoConfig lcDaysPerPage
  where
    mustNoPathInfo = do
      path' <- rqPathInfo <$> getRequest
      unless (BS.null path') pass

handleDay :: T.Text -> LupoHandler ()
handleDay = parseQuery $ A.try multiDaysResponse
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
          day <- db ^! LE.selectPage reqDay
          let nav = N.makeNavigation db reqDay
          notice <- getNoticeDB >>= perform Notice.popAllNotice
          renderView $ V.singleDayView day nav (LE.Comment "" "") notice []

handleEntries :: LupoHandler ()
handleEntries = method GET $ do
  i <- paramId
  (page, entry) <- withEntryDB $ \(LE.EDBWrapper db) -> do
    entry' <- db ^! LE.selectOne i
    page' <- db ^! LE.selectPage (entry' ^. LE.getCreatedDay)
    pure (page', entry')
  redirect =<< U.getURL (U.entryDayPath page entry)

handleSearch :: LupoHandler ()
handleSearch = do
  word <- textParam "word"
  es <- withEntryDB $ \(LE.EDBWrapper db) -> do
    run_ $ db ^. LE.search word $$ EL.consume
  renderView $ V.searchResultView word es

handleComment :: LupoHandler ()
handleComment = method POST $ do
  reqDay <- parseRequestDay
  comment <- LE.Comment <$> textParam "name" <*> textParam "body"
  cond <- try $ withEntryDB $ \(LE.EDBWrapper db) ->
    db ^! LE.insertComment reqDay comment
  case cond of
    Left (InvalidField msgs) -> withEntryDB $ \(LE.EDBWrapper db) -> do
      page <- db ^! LE.selectPage reqDay
      let nav = N.makeNavigation db reqDay
      renderView $ V.singleDayView page nav comment [] msgs
    Right _ -> do
      getNoticeDB >>= perform (Notice.addNotice "Your comment was posted successfully.")
      redirect =<< U.getURL (U.newCommentPath reqDay)
  where
    parseRequestDay = do
      t <- textParam "day"
      either (fail . show) pure $ A.parseOnly dayParser t

handleFeed :: LupoHandler ()
handleFeed = method GET $ do
  entries <- withEntryDB $ \(LE.EDBWrapper db) ->
    E.run_ $ db ^. LE.selectAll $$ EL.take 10
  renderView $ V.entriesFeed entries

monthResponse :: A.Parser (LupoHandler ())
monthResponse = do
  reqMonth <- monthParser
  pure $ withEntryDB $ \(LE.EDBWrapper db) -> do
    let nav = N.makeNavigation db reqMonth
    days <- run_ $ db ^. LE.afterSavedDays reqMonth
                $$ toDayContents db
                =$ takeSameMonthDays reqMonth
    renderView $ V.monthView nav days
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
  let nav = N.makeNavigation db from'
  targetDays <- run_ $ db ^. LE.beforeSavedDays from' $$ EL.take nDays
  pages <- Prelude.mapM (\d -> db ^! LE.selectPage d) targetDays
  renderView $ V.multiDaysView nav pages

dayParser :: A.Parser Time.Day
dayParser = Time.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)

number :: A.Parser Char
number = A.satisfy C.isDigit
