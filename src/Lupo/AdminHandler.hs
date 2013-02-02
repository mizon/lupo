{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Lupo.AdminHandler
  ( handleLogin
  , handleAdmin
  , handleInitAccount
  , handleNewEntry
  , handleEditEntry
  , handleDeleteEntry
  ) where

import Data.Enumerator hiding (replicate, sequence, mapM)
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Auth as A

import Lupo.Application
import qualified Lupo.Database as LDB
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.View as View

handleLogin :: LupoHandler ()
handleLogin =
      method GET showLoginForm
  <|> method POST authenticate
  where
    showLoginForm = do
      cond <- with auth $ A.isLoggedIn
      if cond then
        redirect =<< U.getURL U.adminPath
      else
        View.renderPlain View.loginView

    authenticate = do
      name <- bsParam "name"
      pass' <- A.ClearText <$> bsParam "pass"
      authResult <- with auth $ A.loginByUsername name pass' True
      loginPath <- U.getURL U.loginPath
      adminPath <- U.getURL U.adminPath
      redirect $ either (const loginPath) (const adminPath) authResult

handleAdmin :: LupoHandler ()
handleAdmin = requireAuth $ do
  db <- LDB.getDatabase
  dayContents <- mapM (LDB.selectDay db) =<< getAllDays db
  View.renderAdmin $ View.adminView dayContents
  where
    getAllDays db = do
      (zonedDay -> today) <- liftIO $ Time.getZonedTime
      run_ =<< (EL.consume >>==) <$> LDB.beforeSavedDays db today

handleInitAccount :: LupoHandler ()
handleInitAccount = do
  exists <- with auth $ A.usernameExists "admin"
  when exists pass
  method GET getInitAccountForm <|> method POST registerNewAccount
  where
    getInitAccountForm =
      View.renderPlain View.initAccountView

    registerNewAccount = do
      pass' <- bsParam "pass"
      void $ with auth $ A.createUser "admin" pass'
      redirect =<< U.getURL U.adminPath

handleNewEntry :: LupoHandler ()
handleNewEntry = requireAuth $
      method GET (View.renderAdmin =<< getEditor (LDB.Entry "" ""))
  <|> method POST submitEntry
  where
    submitEntry = do
      action <- textParam "action"
      entry <- LDB.Entry <$> textParam "title" <*> textParam "body"
      case action of
        "Submit" -> do
          db <- LDB.getDatabase
          LDB.insert db entry
          redirect =<< U.getURL U.adminPath
        "Preview" -> View.renderAdmin =<< getPreview <$> dummySaved entry
        "Edit" -> View.renderAdmin =<< getEditor entry
        _ -> error "invalid request"

    getEditor entry = do
      saved <- dummySaved entry
      pure $ View.entryEditorView saved "New" $ flip U.fullPath "admin/new"

    getPreview entry = View.entryPreviewView entry "New" $ flip U.fullPath "admin/new"

    dummySaved entry = do
      today <- liftIO Time.getZonedTime
      pure LDB.Saved
        { LDB.idx = undefined
        , LDB.createdAt = today
        , LDB.modifiedAt = undefined
        , LDB.savedContent = entry
        }

handleEditEntry :: LupoHandler ()
handleEditEntry = requireAuth $
      method GET showEntryEditor
  <|> method POST updateEntry
  where
    showEntryEditor = do
      db <- LDB.getDatabase
      id' <- paramId
      entry <- LDB.select db id'
      View.renderAdmin =<< getEditor entry

    updateEntry = do
      action <- textParam "action"
      db <- LDB.getDatabase
      id' <- paramId
      entry <- LDB.Entry <$> textParam "title" <*> textParam "body"
      baseEntry <- LDB.select db id'
      case action of
        "Submit" -> do
          LDB.update db id' entry
          redirect =<< U.getURL U.adminPath
        "Preview" -> View.renderAdmin =<< getPreview baseEntry {LDB.savedContent = entry}
        "Edit" -> View.renderAdmin =<< getEditor baseEntry {LDB.savedContent = entry}
        _ -> undefined

    getEditor entry = pure
                    $ View.entryEditorView entry "Edit"
                    $ flip U.entryEditPath entry

    getPreview entry = pure
                     $ View.entryPreviewView entry "Edit"
                     $ flip U.entryEditPath entry

handleDeleteEntry :: LupoHandler ()
handleDeleteEntry = requireAuth $ do
  db <- LDB.getDatabase
  LDB.delete db =<< paramId
  redirect =<< U.getURL U.adminPath

requireAuth :: LupoHandler a -> LupoHandler a
requireAuth h = do
  stat <- with auth A.isLoggedIn
  if stat then
    h
  else
    redirect =<< U.getURL U.loginPath
