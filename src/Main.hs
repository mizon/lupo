{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Lupo.AdminHandler as Admin
import qualified Lupo.IndexHandler as Index
import qualified Lupo.EntryDB as EDB
import Lupo.Util
import Lupo.Application
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import qualified Snap.Snaplet.Heist as H
import Snap.Util.FileServe
import qualified Snap.Http.Server.Config as C
import Snap
import Prelude hiding (filter)

main :: IO ()
main = serveSnaplet C.defaultConfig lupoInit

lupoInit :: SnapletInit Lupo Lupo
lupoInit = makeSnaplet "lupo" "A personal web diary." Nothing $ do
    h <- nestSnaplet "heist" heist $ H.heistInit "templates"
    db <- liftIO $ EDB.makeEntryDB
        <$> (DB.ConnWrapper <$> Sqlite3.connectSqlite3 "./development.sqlite3")
        <*> pure "./db/entries"
    addRoutes
        [ ("", Index.top)
        , ("admin", Admin.admin)
        , ("admin/new", Admin.newEntry)
        , ("admin/:id/edit", Admin.editEntry)
        , ("admin/:id/delete", Admin.deleteEntry)
        , ("js", serveDirectory "static/js")
        , ("css", serveDirectory "static/css")
        , (":query", Index.parseQuery =<< param "query")
        ]
    return $ Lupo h db
