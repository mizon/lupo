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
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as BS

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
top =
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
newEntry = method GET (newEntryEditor Nothing) <|> method POST submitEntry
  where
    submitEntry = do
        entry <- EDB.Entry <$> param "title" <*> param "body"
        action <- param "action"
        case action of
            "Submit" -> do
                db <- EDB.getEntryDB
                EDB.insert db entry
                redirect "/admin"
            "Preview" ->
                showPreview "New Entry: Preview" entry
            "Edit" -> newEntryEditor $ Just entry
            _ -> error "invalid request"

    newEntryEditor entry = showEditor "New Entry" entry

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
        entry <- EDB.Entry <$> param "title" <*> param "body"
        action <- param "action"
        case action of
            "Submit" -> do
                db <- EDB.getEntryDB
                EDB.update db id_ entry
                redirect "/admin"
            "Preview" -> showPreview "Edit Entry: Preview" entry
            "Edit" -> editEntryEditor entry
            _ -> undefined

    editEntryEditor entry = showEditor "Edit Entry" $ Just entry

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

showPreview :: T.Text -> EDB.Entry -> Handler Lupo Lupo ()
showPreview prevTitle e@EDB.Entry {..} = do
    (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
    H.renderWithSplices "preview-entry"
        [ ("style-sheet", textSplice "admin")
        , ("preview-title", textSplice prevTitle)
        , ("submit-path", textSplice submitPath)
        , ("entry-title", textSplice title)
        , ("entry-body", textSplice body)
        , ("rendered-body", H.liftHeist $ V.entryBody e)
        ]

showEditor :: T.Text -> Maybe EDB.Entry -> Handler Lupo Lupo ()
showEditor title entry = do
    (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
    H.renderWithSplices "edit-entry"
        [ ("style-sheet", textSplice "admin")
        , ("edit-title", textSplice title)
        , ("submit-path", textSplice submitPath)
        , ("default-title", textSplice $ maybe "" EDB.title entry)
        , ("default-body", textSplice $ maybe "" EDB.body entry)
        ]

textSplice :: T.Text -> H.SnapletSplice Lupo Lupo
textSplice = H.liftHeist . TH.textSplice
