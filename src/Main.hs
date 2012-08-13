{-# LANGUAGE OverloadedStrings
    , TemplateHaskell
    , FlexibleInstances
    , TypeSynonymInstances
    , ViewPatterns
    , RecordWildCards #-}
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
import Data.Maybe
import Data.Monoid

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
        [ ("page-title", textSplice "Lupo Web Diary")
        , ("style-sheet", textSplice "diary")
        ]

admin :: Handler Lupo Lupo ()
admin = do
    db <- EDB.getEntryDB
    entries <- run_ =<< ($$) <$> EDB.all db <*> pure EL.consume
    H.renderWithSplices "admin"
        [ ("entries-list", H.liftHeist $ V.entryInfo entries)
        , ("style-sheet", textSplice "admin")
        ]

newEntry :: Handler Lupo Lupo ()
newEntry = method GET (newEntryEditor Nothing) <|> submitEntry
  where
    submitEntry = method POST $ do
        (TL.fromStrict -> title) <- param "title"
        (TL.fromStrict -> body) <- param "body"
        action <- param "action"
        case action of
            "Submit" -> do
                db <- EDB.getEntryDB
                EDB.insert db $ EDB.Entry title body
                redirect "/admin"
            "Preview" -> do
                spath <- submitPath
                showPreview "New Entry: Preview" spath $ EDB.Entry title body
            "Edit" -> newEntryEditor $ Just $ EDB.Entry title body
            _ -> error "invalid request"

    newEntryEditor :: Maybe EDB.Entry -> Handler Lupo Lupo ()
    newEntryEditor entry = submitPath >>= \s -> showEditor "New Entry" s entry

    submitPath :: MonadSnap m => m BS.ByteString
    submitPath = getsRequest rqURI

editEntry :: Handler Lupo Lupo ()
editEntry = method GET editor <|> method POST updateEntry
  where
    editor = do
        id_ <- paramId
        db <- EDB.getEntryDB
        (EDB.refObject -> entry) <- EDB.select db id_
        editEntryEditor entry

    updateEntry = do
        id_ <- paramId
        (TL.fromStrict -> title) <- param "title"
        (TL.fromStrict -> body) <- param "body"
        action <- param "action"
        case action of
            "Submit" -> do
                db <- EDB.getEntryDB
                EDB.update db id_ $ EDB.Entry title body
                redirect "/admin"
            "Preview" -> submitPath >>= \s -> showPreview "Edit Entry: Preview" s $ EDB.Entry title body
            "Edit" -> editEntryEditor $ EDB.Entry title body
            _ -> undefined

    editEntryEditor entry = submitPath >>= \s -> showEditor "Edit Entry" s $ Just entry
    submitPath = getsRequest rqURI

deleteEntry :: Handler Lupo Lupo ()
deleteEntry = do
    db <- EDB.getEntryDB
    EDB.delete db =<< paramId
    redirect "/admin"

paramId :: (MonadSnap m, Integral a) => m a
paramId = either undefined id . toIntegral <$> param "id"
  where
    toIntegral = A.parseOnly A.decimal

param :: MonadSnap m => BS.ByteString -> m T.Text
param name = maybe undefined TE.decodeUtf8 <$> getParam name

showPreview :: T.Text -> BS.ByteString -> EDB.Entry -> Handler Lupo Lupo ()
showPreview prevTitle (TE.decodeUtf8 -> submitPath) e@EDB.Entry {..} = H.renderWithSplices "preview-entry"
    [ ("style-sheet", textSplice "admin")
    , ("preview-title", textSplice prevTitle)
    , ("submit-path", textSplice submitPath)
    , ("entry-title", textSplice $ TL.toStrict title)
    , ("entry-body", textSplice $ TL.toStrict body)
    , ("rendered-body", H.liftHeist $ V.entryBody e)
    ]

showEditor :: T.Text -> BS.ByteString -> Maybe EDB.Entry -> Handler Lupo Lupo ()
showEditor title (TE.decodeUtf8 -> submitPath) entry = H.renderWithSplices "edit-entry"
    [ ("style-sheet", textSplice "admin")
    , ("edit-title", textSplice title)
    , ("submit-path", textSplice submitPath)
    , ("default-title", textSplice $ maybe "" (TL.toStrict . EDB.title) entry)
    , ("default-body", textSplice $ maybe "" (TL.toStrict . EDB.body) entry)
    ]

textSplice :: T.Text -> H.SnapletSplice Lupo Lupo
textSplice = H.liftHeist . TH.textSplice
