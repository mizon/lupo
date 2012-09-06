{-# LANGUAGE ViewPatterns
    , RecordWildCards
    , OverloadedStrings #-}
module Lupo.AdminHandler
    ( admin
    , newEntry
    , editEntry
    , deleteEntry
    ) where

import Lupo.Application
import Lupo.Util
import qualified Lupo.EntryDB as EDB
import qualified Lupo.View as V
import qualified Snap.Snaplet.Heist as H
import qualified Data.Enumerator.List as EL
import Data.Enumerator hiding (replicate, sequence)
import Snap
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Prelude hiding (filter)

admin :: Handler Lupo Lupo ()
admin = do
    db <- EDB.getEntryDB
    entries <- run_ =<< ($$) <$> EDB.all db <*> pure EL.consume
    H.renderWithSplices "admin"
        [ ("entries-list", pure $ V.entryInfo entries)
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

showPreview :: T.Text -> EDB.Entry -> Handler Lupo Lupo ()
showPreview prevTitle e@EDB.Entry {..} = do
    (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
    H.renderWithSplices "preview-entry"
        [ ("style-sheet", textSplice "admin")
        , ("preview-title", textSplice prevTitle)
        , ("submit-path", textSplice submitPath)
        , ("entry-title", textSplice title)
        , ("entry-body", textSplice body)
        , ("rendered-body", pure $ V.entryBody e)
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
