module Lupo.Backends.View
  ( makeViewFactory
  ) where

import Lupo.Application
import qualified Lupo.Backends.View.Views as V
import Lupo.View

makeViewFactory :: ViewFactory LupoHandler
makeViewFactory = ViewFactory
  { _singleDayView = V.singleDayView
  , _multiDaysView = V.multiDaysView
  , _monthView = V.monthView
  , _searchResultView = V.searchResultView
  , _loginView = V.loginView
  , _initAccountView = V.initAccountView
  , _adminView = V.adminView
  , _entryEditorView = V.entryEditorView
  , _entryPreviewView = V.entryPreviewView
  , _entriesFeed = V.entriesFeed
  }
