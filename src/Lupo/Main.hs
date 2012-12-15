{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Lupo.Main (
    main
  , lupoInit
  ) where

import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Prelude hiding (filter)
import Snap
import qualified Snap.Http.Server.Config as C
import qualified Snap.Snaplet.Auth as A
import qualified Snap.Snaplet.Auth.Backends.JsonFile as JsonFile
import qualified Snap.Snaplet.Heist as H
import qualified Snap.Snaplet.Session.Backends.CookieSession as Cookie
import Snap.Util.FileServe
import Text.XmlHtml

import qualified Lupo.Locale as L
import qualified Lupo.AdminHandler as Admin
import Lupo.Application
import Lupo.Config
import qualified Lupo.Database as EDB
import qualified Lupo.Notice as N
import qualified Lupo.PublicHandler as Public
import qualified Lupo.URLMapper as U
import Lupo.Util

main :: IO ()
main = serveSnaplet C.defaultConfig $ lupoInit LupoConfig {
    lcSiteTitle = "Lupo Web Diary"
  , lcSqlitePath = "./development.sqlite3"
  , lcLocaleFile = "./ja.yml"
  , lcDaysPerPage = 5
  , lcFooterBody = [
      Element "p" [] [
        TextNode "Powered by "
      , Element "a" [("href", "http://www.haskell.org/haskellwiki/Haskell")] [TextNode "Haskell"]
      , TextNode ", "
      , Element "a" [("href", "http://snapframework.com/")] [TextNode "Snap Framework"]
      ]
    ]
  , lcBasePath = ""
  }

lupoInit :: LupoConfig -> SnapletInit Lupo Lupo
lupoInit lc@LupoConfig {..} = makeSnaplet "lupo" "A personal web diary." Nothing $ do
  h <- nestSnaplet "heist" heist $ H.heistInit "templates"
  s <- nestSnaplet "session" session $
    Cookie.initCookieSessionManager "site_key.txt" "sess" (Just 3600)
  a <- nestSnaplet "auth" auth $
    JsonFile.initJsonFileAuthManager A.defAuthSettings session "users.json"
  conn <- liftIO $ DB.ConnWrapper <$> Sqlite3.connectSqlite3 lcSqlitePath
  l <- liftIO $ L.loadYamlLocalizer lcLocaleFile
  A.addAuthSplices auth
  addRoutes [
      ("", Public.handleTop)
    , ("admin", Admin.handleAdmin)
    , ("admin/new", Admin.handleNewEntry)
    , ("admin/:id/edit", Admin.handleEditEntry)
    , ("admin/:id/delete", Admin.handleDeleteEntry)
    , ("login", Admin.handleLogin)
    , ("init-account", Admin.handleInitAccount)
    , ("js", serveDirectory "static/js")
    , ("css", serveDirectory "static/css")
    , ("search", Public.handleSearch)
    , ("entries/:id", Public.handleEntries)
    , (":query", Public.handleDay =<< textParam "query")
    , (":day/comment", Public.handleComment)
    ]
  onUnload $ DB.disconnect conn
  H.addSplices [
      ("lupo:top-page-path", H.liftHeist $ U.urlSplice U.topPagePath)
    , ("lupo:search-path", H.liftHeist $ U.urlSplice $ flip U.fullPath "search")
    ]
  pure $ Lupo h s a (EDB.makeDatabase conn) lc l (initNoticeDB conn) $ U.makeURLMapper lcBasePath
  where
    initNoticeDB conn = N.makeNoticeDB conn $ N.makeSessionBackend session
