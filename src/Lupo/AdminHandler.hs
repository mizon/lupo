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
import qualified Lupo.Entry as E
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
        View.render View.loginView

    authenticate = do
      name <- bsParam "name"
      pass' <- A.ClearText <$> bsParam "pass"
      authResult <- with auth $ A.loginByUsername name pass' True
      loginPath <- U.getURL U.loginPath
      adminPath <- U.getURL U.adminPath
      redirect $ either (const loginPath) (const adminPath) authResult

handleAdmin :: LupoHandler ()
handleAdmin = requireAuth $ do
  db <- E.getDatabase
  dayContents <- mapM (E.selectPage db) =<< getAllDays db
  View.render $ View.adminView dayContents
  where
    getAllDays db = do
      today <- zonedDay <$> liftIO Time.getZonedTime
      run_ $ E.beforeSavedDays db today $$ EL.consume

handleInitAccount :: LupoHandler ()
handleInitAccount = do
  exists <- with auth $ A.usernameExists "admin"
  when exists pass
  method GET getInitAccountForm <|> method POST registerNewAccount
  where
    getInitAccountForm = View.render View.initAccountView

    registerNewAccount = do
      pass' <- bsParam "pass"
      void $ with auth $ A.createUser "admin" pass'
      redirect =<< U.getURL U.adminPath

handleNewEntry :: LupoHandler ()
handleNewEntry = requireAuth $ method GET (View.render =<< getEditor (E.Entry "" ""))
                           <|> method POST submitEntry
  where
    submitEntry = do
      action <- textParam "action"
      entry <- E.Entry <$> textParam "title" <*> textParam "body"
      case action of
        "Submit" -> do
          db <- E.getDatabase
          E.insert db entry
          redirect =<< U.getURL U.adminPath
        "Preview" -> View.render =<< getPreview <$> dummySaved entry
        "Edit" -> View.render =<< getEditor entry
        _ -> error "invalid request"

    getEditor entry = do
      saved <- dummySaved entry
      pure $ View.entryEditorView saved "New" $ flip U.fullPath "admin/new"

    getPreview entry = View.entryPreviewView entry "New" $ flip U.fullPath "admin/new"

    dummySaved entry = do
      today <- liftIO Time.getZonedTime
      pure E.Saved
        { E.idx = undefined
        , E.createdAt = today
        , E.modifiedAt = undefined
        , E.savedContent = entry
        }

handleEditEntry :: LupoHandler ()
handleEditEntry = requireAuth $
      method GET showEntryEditor
  <|> method POST updateEntry
  where
    showEntryEditor = do
      db <- E.getDatabase
      id' <- paramId
      entry <- E.selectOne db id'
      View.render =<< getEditor entry

    updateEntry = do
      action <- textParam "action"
      db <- E.getDatabase
      id' <- paramId
      entry <- E.Entry <$> textParam "title" <*> textParam "body"
      baseEntry <- E.selectOne db id'
      case action of
        "Submit" -> do
          E.update db id' entry
          redirect =<< U.getURL U.adminPath
        "Preview" -> View.render =<< getPreview baseEntry {E.savedContent = entry}
        "Edit" -> View.render =<< getEditor baseEntry {E.savedContent = entry}
        _ -> undefined

    getEditor entry = pure
                    $ View.entryEditorView entry "Edit"
                    $ flip U.entryEditPath entry

    getPreview entry = pure
                     $ View.entryPreviewView entry "Edit"
                     $ flip U.entryEditPath entry

handleDeleteEntry :: LupoHandler ()
handleDeleteEntry = requireAuth $ do
  db <- E.getDatabase
  E.delete db =<< paramId
  redirect =<< U.getURL U.adminPath

requireAuth :: LupoHandler a -> LupoHandler a
requireAuth h = do
  stat <- with auth A.isLoggedIn
  if stat then
    h
  else
    redirect =<< U.getURL U.loginPath
