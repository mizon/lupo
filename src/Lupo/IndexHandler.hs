{-# LANGUAGE OverloadedStrings
    , ViewPatterns
    , ScopedTypeVariables #-}
module Lupo.IndexHandler
    ( top
    , parseQuery
    ) where

import Lupo.Application
import Lupo.Util
import Snap
import qualified Lupo.EntryDB as EDB
import qualified Lupo.View as V
import qualified Text.Templating.Heist as TH
import qualified Snap.Snaplet.Heist as H
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
    days today 5
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
            enumEntries <- EDB.all db
            (fromMaybe [] -> es) <- run_ $ enumEntries
                $= EL.filter ((<= day) . EDB.getCreatedDay)
                $$ packByDay
                =$ EL.head
            H.renderWithSplices "index"
                [ ("page-title", textSplice "")
                , ("style-sheet", textSplice "diary")
                , ("entries", H.liftHeist $ TH.mapSplices V.entry es)
                ]

    dayParser = Ti.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)
    number = A.satisfy C.isDigit

days :: Ti.Day -> Integer -> Handler Lupo Lupo ()
days from nDays = do
    db <- EDB.getEntryDB
    enumEntries <- EDB.all db
    ess <- run_ $ enumEntries
        $= EL.filter ((<= from) . EDB.getCreatedDay)
        $$ packByDay
        =$ EL.take nDays
    H.renderWithSplices "index"
        [ ("page-title", textSplice "Lupo Web Diary")
        , ("style-sheet", textSplice "diary")
        , ("entries", H.liftHeist $ TH.mapSplices (\es -> V.day (EDB.getCreatedDay $ head es) es) ess)
        ]

packByDay :: Monad m => Enumeratee (EDB.Saved a) [EDB.Saved a] m b
packByDay = E.sequence $ EL.head >>= maybe (pure []) (\h -> do
      es <- EL.takeWhile $ isSameCreatedDay h
      return $ h : es
    )
  where
    isSameCreatedDay a b = EDB.getCreatedDay a == EDB.getCreatedDay b
