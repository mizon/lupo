module Lupo.AdminHandler
  ( handleLogin
  , handleAdmin
  , handleNewEntry
  , handleEditEntry
  , handleDeleteEntry
  ) where

import Data.Enumerator hiding (replicate, sequence, mapM)
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time
import Prelude hiding (filter)
import Snap
import Control.Monad.CatchIO

import Lupo.Application
import qualified Lupo.Auth as A
import qualified Lupo.Entry as E
import Lupo.Import
import qualified Lupo.URLMapper as U
import Lupo.Util
import qualified Lupo.View as V

requireAuth :: LupoHandler a -> LupoHandler a
requireAuth h = do
  stat <- with auth A.isLoggedIn
  if stat then
    h
  else
    redirect =<< U.getURL U.loginPath

handleLogin :: LupoHandler ()
handleLogin = method GET showLoginForm
          <|> method POST authenticate
  where
    showLoginForm = do
      challenge <- with auth $ do
        loggedIn <- A.isLoggedIn
        when loggedIn A.logout
        A.prepareChallenge
      renderView $ V.loginView challenge

    authenticate = do
      pass' <- textParam "pass"
      with auth (A.login pass') `catch` \(_ :: LoginFailed) -> do
        redirect =<< U.getURL U.loginPath
      redirect =<< U.getURL U.adminPath

handleAdmin :: LupoHandler ()
handleAdmin = requireAuth $ do
  dayContents <- withEntryDB $ \(E.EDBWrapper db) ->
    mapM (\d -> db ^! E.selectPage d) =<< getAllDays db
  renderView $ V.adminView dayContents
  where
    getAllDays db = do
      today <- zonedDay <$> liftIO Time.getZonedTime
      run_ $ db ^. E.beforeSavedDays today $$ EL.consume

handleNewEntry :: LupoHandler ()
handleNewEntry = requireAuth $ method GET (V.render =<< getEditor (E.Entry "" ""))
                           <|> method POST submitEntry
  where
    submitEntry = do
      entry <- E.Entry <$> textParam "title" <*> textParam "body"
      textParam "action" >>= \case
        "Submit" -> do
          withEntryDB $ \(E.EDBWrapper db) ->
            db ^! E.insert entry
          redirect =<< U.getURL U.adminPath
        "Preview" -> V.render =<< getPreview =<< dummySaved entry
        "Edit" -> V.render =<< getEditor entry
        _ -> error "invalid request"

    getEditor entry = do
      saved <- dummySaved entry
      use $ viewFactory . V.entryEditorView saved "New" (U.fullPath "admin/new")

    getPreview entry = use $ viewFactory . V.entryPreviewView entry "New" (U.fullPath "admin/new")

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
    showEntryEditor = do
      id' <- paramId
      entry <- withEntryDB $ \(E.EDBWrapper db) ->
        db ^! E.selectOne id'
      renderView $ getEditor entry

    updateEntry = do
      id' <- paramId
      entry <- E.Entry <$> textParam "title" <*> textParam "body"
      baseEntry <- withEntryDB $ \(E.EDBWrapper db) ->
        db ^! E.selectOne id'
      textParam "action" >>= \case
        "Submit" -> do
          withEntryDB $ \(E.EDBWrapper db) -> do
            db ^! E.update id' entry
          redirect =<< U.getURL U.adminPath
        "Preview" -> renderView $ getPreview $ baseEntry & E.savedContent .~ entry
        "Edit" -> renderView $ getEditor $ baseEntry & E.savedContent .~ entry
        _ -> undefined

    getEditor entry = V.entryEditorView entry "Edit" $ U.entryEditPath entry
    getPreview entry = V.entryPreviewView entry "Edit" $ U.entryEditPath entry

handleDeleteEntry :: LupoHandler ()
handleDeleteEntry = requireAuth $ do
  i <- paramId
  withEntryDB $ \(E.EDBWrapper db) ->
    db ^! E.delete i
  redirect =<< U.getURL U.adminPath
