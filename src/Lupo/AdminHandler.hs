{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import Lupo.Import
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.View as V

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
        V.render . V.loginView =<< gets viewFactory

    authenticate = do
      name <- bsParam "name"
      pass' <- A.ClearText <$> bsParam "pass"
      authResult <- with auth $ A.loginByUsername name pass' True
      loginPath <- U.getURL U.loginPath
      adminPath <- U.getURL U.adminPath
      redirect $ either (const loginPath) (const adminPath) authResult

handleAdmin :: LupoHandler ()
handleAdmin = requireAuth $ withEntryDB $ \(E.EDBWrapper db) -> do
  vf <- gets viewFactory
  dayContents <- mapM (\d -> db ^! E.selectPage d) =<< getAllDays db
  V.render $ V.adminView vf dayContents
  where
    getAllDays db = do
      today <- zonedDay <$> liftIO Time.getZonedTime
      run_ $ db ^. E.beforeSavedDays today $$ EL.consume

handleInitAccount :: LupoHandler ()
handleInitAccount = do
  exists <- with auth $ A.usernameExists "admin"
  when exists pass
  method GET getInitAccountForm <|> method POST registerNewAccount
  where
    getInitAccountForm = do
      vf <- gets viewFactory
      V.render $ V.initAccountView vf

    registerNewAccount = do
      pass' <- bsParam "pass"
      void $ with auth $ A.createUser "admin" pass'
      redirect =<< U.getURL U.adminPath

handleNewEntry :: LupoHandler ()
handleNewEntry = requireAuth $ method GET (V.render =<< getEditor (E.Entry "" ""))
                           <|> method POST submitEntry
  where
    submitEntry = do
      entry <- E.Entry <$> textParam "title" <*> textParam "body"
      textParam "action" >>=
        \case
          "Submit" -> withEntryDB $ \(E.EDBWrapper db) -> do
            db ^! E.insert entry
            redirect =<< U.getURL U.adminPath
          "Preview" -> do
             p <- getPreview =<< dummySaved entry
             V.render p
          "Edit" -> V.render =<< getEditor entry
          _ -> error "invalid request"

    getEditor entry = do
      vf <- gets viewFactory
      saved <- dummySaved entry
      pure $ V.entryEditorView vf saved "New" $ U.fullPath "admin/new"

    getPreview entry = do
      vf <- gets viewFactory
      pure $ V.entryPreviewView vf entry "New" $ U.fullPath "admin/new"

    dummySaved entry = do
      today <- liftIO Time.getZonedTime
      pure E.Saved
        { _idx = undefined
        , _createdAt = today
        , _modifiedAt = undefined
        , _savedContent = entry
        }

handleEditEntry :: LupoHandler ()
handleEditEntry = requireAuth $ method GET showEntryEditor
                            <|> method POST updateEntry
  where
    showEntryEditor = withEntryDB $ \(E.EDBWrapper db) -> do
      id' <- paramId
      entry <- db ^! E.selectOne id'
      V.render =<< getEditor entry

    updateEntry = do
      withEntryDB $ \(E.EDBWrapper db) -> do
        id' <- paramId
        entry <- E.Entry <$> textParam "title" <*> textParam "body"
        baseEntry <- db ^! E.selectOne id'
        textParam "action" >>= \case
          "Submit" -> do
            db ^! E.update id' entry
            redirect =<< U.getURL U.adminPath
          "Preview" -> V.render =<< getPreview (baseEntry & E.savedContent .~ entry)
          "Edit" -> V.render =<< getEditor (baseEntry & E.savedContent .~ entry)
          _ -> undefined

    getEditor entry = do
      vf <- gets viewFactory
      pure $ V.entryEditorView vf entry "Edit" $ U.entryEditPath entry

    getPreview entry = do
      vf <- gets viewFactory
      pure $ V.entryPreviewView vf entry "Edit" $ U.entryEditPath entry

handleDeleteEntry :: LupoHandler ()
handleDeleteEntry = requireAuth $ withEntryDB $ \(E.EDBWrapper db) -> do
  i <- paramId
  db ^! E.delete i
  redirect =<< U.getURL U.adminPath

requireAuth :: LupoHandler a -> LupoHandler a
requireAuth h = do
  stat <- with auth A.isLoggedIn
  if stat then
    h
  else
    redirect =<< U.getURL U.loginPath
