module Lupo.Backends.View
  ( makeViewFactory
  ) where

import Lupo.Application
import qualified Lupo.Backends.View.Views as V
import Lupo.View

makeViewFactory :: ViewFactory LupoHandler
makeViewFactory = ViewFactory
  { singleDayView = V.singleDayView
  , multiDaysView = V.multiDaysView
  , monthView = V.monthView
  , searchResultView = V.searchResultView
  , loginView = V.loginView
  , initAccountView = V.initAccountView
  , adminView = V.adminView
  , entryEditorView = V.entryEditorView
  , entryPreviewView = V.entryPreviewView
  , entriesFeed = V.entriesFeed
  }
