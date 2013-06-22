module Lupo.View
  ( View (..)
  , ViewFactory (..)
  ) where

import qualified Data.Text as T
import qualified Heist as H

import qualified Lupo.Entry as E
import qualified Lupo.Navigation as N
import qualified Lupo.URLMapper as U

data View h = View
  { render :: h ()
  }

data ViewFactory h = ViewFactory
  { singleDayView :: E.Page -> N.Navigation (H.HeistT h h) -> E.Comment -> [T.Text] -> [T.Text] -> View h
  , multiDaysView :: N.Navigation (H.HeistT h h) -> [E.Page] -> View h
  , monthView :: N.Navigation (H.HeistT h h) -> [E.Page] -> View h
  , searchResultView :: T.Text -> [E.Saved E.Entry] -> View h
  , loginView :: View h
  , initAccountView :: View h
  , adminView :: [E.Page] -> View h
  , entryEditorView :: E.Saved E.Entry -> T.Text -> (U.URLMapper -> U.Path) -> View h
  , entryPreviewView :: E.Saved E.Entry -> T.Text -> (U.URLMapper -> U.Path) -> View h
  , entriesFeed :: [E.Saved E.Entry] -> View h
  }
