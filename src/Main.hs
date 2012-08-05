{-# LANGUAGE OverloadedStrings
    , TemplateHaskell
    , FlexibleInstances
    , TypeSynonymInstances #-}
module Main (main) where

import qualified Lupo.EntryDB as EDB
import qualified Text.Templating.Heist as TH
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import qualified Snap.Snaplet.Heist as H
import Snap.Util.FileServe
import qualified Snap.Http.Server.Config as C
import Snap

import Development.Placeholders

data Lupo = Lupo
    { _heist :: Snaplet (H.Heist Lupo)
    , entryDB :: EDB.EntryDB
    }
makeLens ''Lupo

instance H.HasHeist Lupo where
    heistLens = subSnaplet heist

main :: IO ()
main = serveSnaplet C.defaultConfig lupoInit

lupoInit :: SnapletInit Lupo Lupo
lupoInit = makeSnaplet "lupo" "A personal web diary." Nothing $ do
    h <- nestSnaplet "heist" heist  $ H.heistInit "templates"
    db <- liftIO $ EDB.makeEntryDB <$> (DB.ConnWrapper <$> Sqlite3.connectSqlite3 ":memory:") <*> pure ""
    addRoutes
        [ ("", top)
        , ("js", serveDirectory "static/js")
        , ("css", serveDirectory "static/css")
        ]
    return $ Lupo h db

instance EDB.MonadEntryDB (Handler Lupo Lupo) where
    getEntryDB = gets entryDB

top :: Handler Lupo Lupo ()
top = do
    db <- EDB.getEntryDB
    e <- EDB.select db 100
    H.renderWithSplices "index"
        [ ("page-title", H.liftHeist $ TH.textSplice "Lupo Web Diary System")
        ]

entryIndex :: Snap ()
entryIndex = $notImplemented

entry :: Snap ()
entry = $notImplemented

searchEntry :: Snap ()
searchEntry = $notImplemented

echoHandler :: Snap ()
echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
        writeBS param
