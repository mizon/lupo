module Lupo.View
  ( View (..)
  , ViewFactory (..)
  , singleDayView
  , multiDaysView
  , monthView
  , searchResultView
  , loginView
  , initAccountView
  , adminView
  , entryEditorView
  , entryPreviewView
  , entriesFeed
  ) where

import qualified Data.Text as T
import qualified Heist as H

import qualified Lupo.Entry as E
import Lupo.Import
import qualified Lupo.Navigation as N
import qualified Lupo.URLMapper as U

data View h = View
  { render :: h ()
  }

data ViewFactory h = ViewFactory
  { _singleDayView :: E.Page -> N.Navigation (H.HeistT h h) -> E.Comment -> [T.Text] -> [T.Text] -> View h
  , _multiDaysView :: N.Navigation (H.HeistT h h) -> [E.Page] -> View h
  , _monthView :: N.Navigation (H.HeistT h h) -> [E.Page] -> View h
  , _searchResultView :: T.Text -> [E.Saved E.Entry] -> View h
  , _loginView :: View h
  , _initAccountView :: View h
  , _adminView :: [E.Page] -> View h
  , _entryEditorView :: E.Saved E.Entry -> T.Text -> Getter U.URLMapper U.Path -> View h
  , _entryPreviewView :: E.Saved E.Entry -> T.Text -> Getter U.URLMapper U.Path -> View h
  , _entriesFeed :: [E.Saved E.Entry] -> View h
  }

singleDayView :: E.Page -> N.Navigation (H.HeistT h h) -> E.Comment -> [T.Text] -> [T.Text] -> Getter (ViewFactory h) (View h)
singleDayView p n c notice errs = to $ \self ->
  _singleDayView self p n c notice errs

multiDaysView :: N.Navigation (H.HeistT h h) -> [E.Page] -> Getter (ViewFactory h) (View h)
multiDaysView n ps = to $ \self ->
  _multiDaysView self n ps

monthView :: N.Navigation (H.HeistT h h) -> [E.Page] -> Getter (ViewFactory h) (View h)
monthView n ps = to $ \self ->
  _monthView self n ps

searchResultView :: T.Text -> [E.Saved E.Entry] -> Getter (ViewFactory h) (View h)
searchResultView t es = to $ \self ->
  _searchResultView self t es

loginView :: Getter (ViewFactory h) (View h)
loginView = to _loginView

initAccountView :: Getter (ViewFactory h) (View h)
initAccountView = to _initAccountView

adminView :: [E.Page] -> Getter (ViewFactory h) (View h)
adminView ps = to $ \self ->
  _adminView self ps

entryEditorView :: E.Saved E.Entry -> T.Text -> Getter U.URLMapper U.Path -> Getter (ViewFactory h) (View h)
entryEditorView e t url = to $ \self ->
  _entryEditorView self e t url

entryPreviewView :: E.Saved E.Entry -> T.Text -> Getter U.URLMapper U.Path -> Getter (ViewFactory h) (View h)
entryPreviewView e t url = to $ \self ->
  _entryPreviewView self e t url

entriesFeed :: [E.Saved E.Entry] -> Getter (ViewFactory h) (View h)
entriesFeed es = to $ \self ->
  _entriesFeed self es
