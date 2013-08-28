module Lupo.Site
  ( lupoInit
  ) where

import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Heist.Interpreted as H
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Heist as H
import qualified Snap.Snaplet.Session.Backends.CookieSession as Cookie
import Snap.Util.FileServe

import qualified Lupo.AdminHandler as Admin
import Lupo.Application
import qualified Lupo.Backends.Auth as A
import qualified Lupo.Backends.Entry as E
import qualified Lupo.Backends.Notice as N
import qualified Lupo.Backends.URLMapper as U
import qualified Lupo.Backends.View as V
import Lupo.Config
import qualified Lupo.ConnectionPool as CP
import Lupo.Import
import qualified Lupo.Locale as L
import qualified Lupo.PublicHandler as Public
import qualified Lupo.URLMapper as U
import Lupo.Util

lupoInit :: LupoConfig -> SnapletInit Lupo Lupo
lupoInit lc = makeSnaplet "lupo" "A personal web diary" Nothing $ do
  h <- nestSnaplet "heist" heist $ H.heistInit "templates"
  s <- nestSnaplet "session" session $ Cookie.initCookieSessionManager "site_key.txt" "sess" Nothing
  a <- nestSnaplet "auth" auth $ A.initAuthenticator session lc
  conn <- liftIO $ DB.ConnWrapper <$> Sqlite3.connectSqlite3 (lc ^. lcSqlitePath)
  edbs <- liftIO $ CP.makeConnectionPool (E.makeEntryDatabase conn $ lc ^. lcSpamFilter) 5
  l <- liftIO $ L.loadYamlLocalizer $ lc ^. lcLocaleFile
  addRoutes
    [ ("", Public.handleTop)
    , ("admin", Admin.handleAdmin)
    , ("admin/new", Admin.handleNewEntry)
    , ("admin/:id/edit", Admin.handleEditEntry)
    , ("admin/:id/delete", Admin.handleDeleteEntry)
    , ("login", Admin.handleLogin)
    , ("js", serveDirectory "static/js")
    , ("css", serveDirectory "static/css")
    , ("images", serveDirectory "static/images")
    , ("search", Public.handleSearch)
    , ("entries/:id", Public.handleEntries)
    , (":query", Public.handleDay =<< textParam "query")
    , (":day/comment", Public.handleComment)
    , ("recent.atom", Public.handleFeed)
    ]
  onUnload $ DB.disconnect conn
  H.addSplices
    [ ("lupo:language", H.textSplice $ lc ^. lcLanguage)
    , ("lupo:top-page-path", U.urlSplice U.topPagePath)
    , ("lupo:search-path", U.urlSplice (U.fullPath "search"))
    ]
  pure Lupo
    { _heist = h
    , _session = s
    , _auth = a
    , _lupoConfig = lc
    , _localizer = l
    , _noticeDB = initNoticeDB conn
    , _urlMapper = U.makeURLMapper $ lc ^. lcBasePath
    , _entryDBPool = edbs
    , _viewFactory = V.makeViewFactory
    }
  where
    initNoticeDB conn = N.makeNoticeDB conn $ N.makeSessionBackend session
