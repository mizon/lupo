{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where

import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Prelude hiding (filter)
import Snap
import qualified Snap.Http.Server.Config as C
import qualified Snap.Snaplet.Heist as H
import Snap.Util.FileServe
import Text.XmlHtml

import qualified Lupo.Locale as L
import qualified Lupo.AdminHandler as Admin
import Lupo.Application
import Lupo.Config
import qualified Lupo.Database as EDB
import qualified Lupo.IndexHandler as Index
import Lupo.Util

main :: IO ()
main = serveSnaplet C.defaultConfig $ lupoInit LupoConfig
    { lcSiteTitle = "Lupo Web Diary"
    , lcSqlitePath = "./development.sqlite3"
    , lcLocaleFile = "./ja.yml"
    , lcDaysPerPage = 5
    , lcFooterBody = [Element "p" []
        [ TextNode "Powered by "
        , Element "a" [("href", "http://www.haskell.org/haskellwiki/Haskell")] [TextNode "Haskell"]
        , TextNode ", "
        , Element "a" [("href", "http://snapframework.com/")] [TextNode "Snap Framework"]
        ]]
    }

lupoInit :: LupoConfig -> SnapletInit Lupo Lupo
lupoInit lc@LupoConfig {..} = makeSnaplet "lupo" "A personal web diary." Nothing $ do
    h <- nestSnaplet "heist" heist $ H.heistInit "templates"
    conn <- liftIO $ DB.ConnWrapper <$> Sqlite3.connectSqlite3 lcSqlitePath
    l <- liftIO $ L.loadYamlLocalizer lcLocaleFile
    addRoutes
        [ ("", Index.topPageHandler)
        , ("admin", Admin.admin)
        , ("admin/new", Admin.newEntry)
        , ("admin/:id/edit", Admin.editEntry)
        , ("admin/:id/delete", Admin.deleteEntry)
        , ("js", serveDirectory "static/js")
        , ("css", serveDirectory "static/css")
        , ("search", Index.searchHandler)
        , (":query", Index.parseQuery =<< param "query")
        ]
    pure $ Lupo h (EDB.makeDatabase conn) lc l
