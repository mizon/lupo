{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
module Lupo.AdminHandler (
    login
  , admin
  , initAccount
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
import qualified Snap.Snaplet.Auth as A
import qualified Snap.Snaplet.Heist as H

import Lupo.Application
import qualified Lupo.Database as LDB
import Lupo.Util
import qualified Lupo.View as View
import qualified Lupo.ViewFragment as V

login :: LupoHandler ()
login =
      method GET showLoginForm
  <|> method POST authenticate
  where
    showLoginForm = do
      cond <- with auth $ A.isLoggedIn
      if cond then
        redirect "/admin"
      else
        View.renderPlain $ View.loginView

    authenticate = do
      name <- bsParam "name"
      pass' <- A.ClearText <$> bsParam "pass"
      authResult <- with auth $ A.loginByUsername name pass' True
      redirect $ either (const "/login") (const "/admin") authResult

admin :: LupoHandler ()
admin = requireAuth $ do
  db <- LDB.getDatabase
  entries <- run_ =<< ($$) <$> LDB.all db <*> pure EL.consume
  H.renderWithSplices "admin" [
      ("lupo:entries-list", pure $ V.entryInfo =<< entries)
    , ("lupo:style-sheet", textSplice "admin")
    ]

initAccount :: LupoHandler ()
initAccount = do
  exists <- with auth $ A.usernameExists "admin"
  when exists pass
  method GET getInitAccountForm <|> method POST registerNewAccount
  where
    getInitAccountForm =
      View.renderPlain View.initAccountView

    registerNewAccount = do
      pass' <- bsParam "pass"
      void $ with auth $ A.createUser "admin" pass'
      redirect "/admin"

newEntry :: LupoHandler ()
newEntry = requireAuth $
      method GET (newEntryEditor Nothing)
  <|> method POST submitEntry
  where
    submitEntry = do
      entry <- LDB.Entry <$> textParam"title" <*> textParam"body"
      action <- textParam"action"
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
editEntry = requireAuth $ method GET editor <|> method POST updateEntry
  where
    editor = do
      id' <- paramId
      db <- LDB.getDatabase
      (LDB.savedContent -> entry) <- LDB.select db id'
      editEntryEditor entry

    updateEntry = do
      id' <- paramId
      entry <- LDB.Entry <$> textParam"title" <*> textParam"body"
      action <- textParam"action"
      case action of
        "Submit" -> do
          db <- LDB.getDatabase
          LDB.update db id' entry
          redirect "/admin"
        "Preview" -> showPreview "Edit Entry: Preview" entry
        "Edit" -> editEntryEditor entry
        _ -> undefined

    editEntryEditor entry = showEditor "Edit Entry" $ Just entry

deleteEntry :: LupoHandler ()
deleteEntry = requireAuth $ do
  db <- LDB.getDatabase
  LDB.delete db =<< paramId
  redirect "/admin"

showPreview :: T.Text -> LDB.Entry -> LupoHandler ()
showPreview prevTitle e@LDB.Entry {..} = do
  (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
  H.renderWithSplices "preview-entry" [
      ("lupo:style-sheet", textSplice "admin")
    , ("lupo:preview-title", textSplice prevTitle)
    , ("lupo:submit-path", textSplice submitPath)
    , ("lupo:entry-title", textSplice entryTitle)
    , ("lupo:entry-body", textSplice entryBody)
    , ("lupo:rendered-body", pure $ V.renderBody e)
    ]

showEditor :: T.Text -> Maybe LDB.Entry -> LupoHandler ()
showEditor title entry = requireAuth $ do
  (TE.decodeUtf8 -> submitPath) <- getsRequest rqURI
  H.renderWithSplices "edit-entry" [
      ("lupo:style-sheet", textSplice "admin")
    , ("lupo:edit-title", textSplice title)
    , ("lupo:submit-path", textSplice submitPath)
    , ("lupo:default-title", textSplice $ maybe "" LDB.entryTitle entry)
    , ("lupo:default-body", textSplice $ maybe "" LDB.entryBody entry)
    ]

requireAuth :: LupoHandler a -> LupoHandler a
requireAuth h = do
  stat <- with auth A.isLoggedIn
  if stat then
    h
  else
    redirect "/login"
