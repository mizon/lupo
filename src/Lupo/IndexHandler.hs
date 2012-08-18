{-# LANGUAGE OverloadedStrings
    , ViewPatterns
    , ScopedTypeVariables #-}
module Lupo.IndexHandler
    ( top
    , entries
    ) where

import Lupo.Application
import Lupo.Util
import qualified Lupo.EntryDB as EDB
import qualified Lupo.View as V
import qualified Text.Templating.Heist as TH
import qualified Snap.Snaplet.Heist as H
import qualified Data.Enumerator.List as EL
import qualified Data.Attoparsec.Text as A
import qualified Data.Time as Ti
import Data.Enumerator as E hiding (replicate)
import Snap
import qualified Data.Char as C
import Control.Monad as M
import System.Locale
import Prelude hiding (filter)

top :: Handler Lupo Lupo ()
top = do
    db <- EDB.getEntryDB
    es <- run_ =<< ($$) <$> EDB.all db <*> pure EL.consume
    H.renderWithSplices "index"
        [ ("page-title", textSplice "Lupo Web Diary")
        , ("style-sheet", textSplice "diary")
        , ("entries", H.liftHeist $ TH.mapSplices V.entry es)
        ]

entries :: Handler Lupo Lupo ()
entries = do
    (from, nDays) <- either (const pass) pure =<< parseQuery <$> param "query"
    db <- EDB.getEntryDB
    enumEntries <- EDB.all db
    (concat -> es) <- run_ $ enumEntries
        $= EL.filter ((<= from) . EDB.getCreatedDay)
        $$ packByDay
        =$ EL.take nDays
    H.renderWithSplices "index"
        [ ("page-title", textSplice "Lupo Web Diary")
        , ("style-sheet", textSplice "diary")
        , ("entries", H.liftHeist $ TH.mapSplices V.entry es)
        ]
  where
    parseQuery = A.parseOnly $ do
        date <- Ti.readTime defaultTimeLocale "%Y%m%d" <$> M.sequence (replicate 8 number)
        void $ A.char '-'
        nentries <- read . pure <$> number
        return (date, nentries)
      where
        number = A.satisfy C.isDigit

packByDay :: Monad m => Enumeratee (EDB.Saved a) [EDB.Saved a] m b
packByDay = E.sequence $ do
    h <- EL.head
    case h of
        Just entry -> do
            follows <- EL.takeWhile $ isSameCreatedDay entry
            return $ entry : follows
        Nothing -> return []
  where
    isSameCreatedDay a b = EDB.getCreatedDay a == EDB.getCreatedDay b
