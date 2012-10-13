{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Lupo.AdminHandler
    ( admin
    , newEntry
    , editEntry
    , deleteEntry
    ) where

import Data.Enumerator hiding (replicate, sequence)
import qualified Data.Enumerator.List as EL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Heist as H

import Lupo.Application
import qualified Lupo.Database as LDB
import Lupo.Util
import qualified Lupo.View as V

admin :: LupoHandler ()
admin = do
    db <- LDB.getDatabase
    entries <- run_ =<< ($$) <$> LDB.all db <*> pure EL.consume
    H.renderWithSplices "admin"
        [ ("entries-list", pure $ V.entryInfo <$> entries)
        , ("style-sheet", textSplice "admin")
        ]

newEntry :: LupoHandler ()
newEntry = method GET (newEntryEditor Nothing) <|> method POST submitEntry
  where
    submitEntry = do
        entry <- LDB.Entry <$> param "title" <*> param "body"
        action <- param "action"
        case action of
            "Submit" -> do
                db <- LDB.getDatabase
                LDB.insert db entry
                redirect "/admin"
            "Preview" ->
                showPreview "New Entry: Preview" entry
            "Edit" -> newEntryEditor $ Just entry
            _ -> error "invalid request"

    newEntryEditor entry = showEditor "New Entry" entry

editEntry :: LupoHandler ()
editEntry = method GET editor <|> method POST updateEntry
  where
    editor = do
        id_ <- paramId
        db <- LDB.getDatabase
        (LDB.refObject -> entry) <- LDB.select db id_
        editEntryEditor entry

    updateEntry = do
        id_ <- paramId
        entry <- LDB.Entry <$> param "title" <*> param "body"
        action <- param "action"
        case action of
            "Submit" -> do
                db <- LDB.getDatabase
                LDB.update db id_ entry
                redirect "/admin"
            "Preview" -> showPreview "Edit Entry: Preview" entry
            "Edit" -> editEntryEditor entry
            _ -> undefined

    editEntryEditor entry = showEditor "Edit Entry" $ Just entry

deleteEntry :: LupoHandler ()
deleteEntry = do
    db <- LDB.getDatabase
    LDB.delete db =<< paramId
    redirect "/admin"

showPreview :: T.Text -> LDB.Entry -> LupoHandler ()
showPreview prevTitle e@LDB.Entry {..} = do
    (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
    H.renderWithSplices "preview-entry"
        [ ("style-sheet", textSplice "admin")
        , ("preview-title", textSplice prevTitle)
        , ("submit-path", textSplice submitPath)
        , ("entry-title", textSplice title)
        , ("entry-body", textSplice body)
        , ("rendered-body", pure $ V.entryBody e)
        ]

showEditor :: T.Text -> Maybe LDB.Entry -> LupoHandler ()
showEditor title entry = do
    (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
    H.renderWithSplices "edit-entry"
        [ ("style-sheet", textSplice "admin")
        , ("edit-title", textSplice title)
        , ("submit-path", textSplice submitPath)
        , ("default-title", textSplice $ maybe "" LDB.title entry)
        , ("default-body", textSplice $ maybe "" LDB.body entry)
        ]
