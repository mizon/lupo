{-# LANGUAGE OverloadedStrings
    , TemplateHaskell
    , FlexibleInstances
    , TypeSynonymInstances
    , ViewPatterns #-}
module Main (main) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.View as V
import qualified Text.Templating.Heist as TH
import qualified Database.HDBC.Sqlite3 as Sqlite3
import qualified Database.HDBC as DB
import qualified Snap.Snaplet.Heist as H
import qualified Data.Enumerator.List as EL
import qualified Data.Attoparsec.Text as A
import Data.Enumerator
import Snap.Util.FileServe
import qualified Snap.Http.Server.Config as C
import Snap
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

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
    db <- liftIO $ EDB.makeEntryDB
        <$> (DB.ConnWrapper <$> Sqlite3.connectSqlite3 "./development.sqlite3")
        <*> pure "./db/entries"
    addRoutes
        [ ("", top)
        , ("admin", admin)
        , ("admin/new", newEntry)
        , ("admin/:id/edit", editEntry)
        , ("admin/:id/delete", deleteEntry)
        , ("js", serveDirectory "static/js")
        , ("css", serveDirectory "static/css")
        ]
    return $ Lupo h db

instance EDB.MonadEntryDB (Handler Lupo Lupo) where
    getEntryDB = gets entryDB

top :: Handler Lupo Lupo ()
top = do
    db <- EDB.getEntryDB
    entries <- run_ =<< ($$) <$> EDB.all db <*> pure EL.consume
    H.renderWithSplices "index"
        [ ("page-title", H.liftHeist $ TH.textSplice "Lupo Web Diary")
        , ("style-sheet", H.liftHeist $ TH.textSplice "diary")
        ]

admin :: Handler Lupo Lupo ()
admin = do
    db <- EDB.getEntryDB
    entries <- run_ =<< ($$) <$> EDB.all db <*> pure EL.consume
    H.renderWithSplices "admin"
        [ ("entries-list", H.liftHeist $ V.entryInfo entries)
        , ("style-sheet", H.liftHeist $ TH.textSplice "admin")
        ]

newEntry :: Handler Lupo Lupo ()
newEntry = method GET showEditor <|> submitEntry
  where
    showEditor = H.renderWithSplices "new-entry"
        [ ("header-title", textSplice "New Entry")
        , ("style-sheet", textSplice "admin")
        ]

    submitEntry = method POST $ do
        (TL.fromStrict -> title) <- param "title"
        (TL.fromStrict -> body) <- param "body"
        db <- EDB.getEntryDB
        EDB.insert db $ EDB.Entry title body
        redirect "/admin"

editEntry :: Handler Lupo Lupo ()
editEntry = method GET showEditor <|> updateEntry
  where
    showEditor = do
        db <- EDB.getEntryDB
        id <- paramId
        (EDB.refObject -> entry) <- EDB.select db id
        H.renderWithSplices "edit-entry"
            [ ("style-sheet", textSplice "admin")
            , ("default-title", textSplice $ TL.toStrict $ EDB.title entry)
            , ("default-body", textSplice $ TL.toStrict $ EDB.body entry)
            , ("entry-id", textSplice $ T.pack $ show id)
            ]

    updateEntry = method POST $ do
        id <- paramId
        (TL.fromStrict -> title) <- param "title"
        (TL.fromStrict -> body) <- param "body"
        db <- EDB.getEntryDB
        EDB.update db id $ EDB.Entry title body
        redirect "/admin"

deleteEntry :: Handler Lupo Lupo ()
deleteEntry = do
    db <- EDB.getEntryDB
    EDB.delete db =<< paramId
    redirect "/admin"

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

paramId :: (MonadSnap m, Integral a) => m a
paramId = either undefined id . toIntegral <$> param "id"
  where
    toIntegral = A.parseOnly A.decimal

param :: MonadSnap m => BS.ByteString -> m T.Text
param name = maybe undefined TE.decodeUtf8 <$> getParam name

textSplice :: T.Text -> H.SnapletSplice Lupo Lupo
textSplice = H.liftHeist . TH.textSplice
