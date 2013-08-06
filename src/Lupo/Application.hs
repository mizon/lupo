{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lupo.Application
  ( Lupo (..)
  , LupoHandler
  , heist
  , session
  , auth
  , getNoticeDB
  , withEntryDB
  , renderView
  ) where

import Control.Monad.CatchIO hiding (Handler)
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Auth as A
import qualified Snap.Snaplet.Heist as SH
import qualified Snap.Snaplet.Session as S

import Lupo.Config
import qualified Lupo.ConnectionPool as C
import qualified Lupo.Entry as E
import Lupo.Import
import qualified Lupo.Locale as L
import qualified Lupo.Notice as N
import qualified Lupo.URLMapper as U
import qualified Lupo.View as V

data Lupo = Lupo
  { _heist :: Snaplet (SH.Heist Lupo)
  , _session :: Snaplet S.SessionManager
  , _auth :: Snaplet (A.AuthManager Lupo)
  , lupoConfig :: LupoConfig
  , localizer :: L.Localizer
  , noticeDB :: forall b. N.NoticeDB (Handler b Lupo)
  , urlMapper :: U.URLMapper
  , entryDBPool :: C.ConnectionPool E.EDBWrapper
  , viewFactory :: V.ViewFactory (Handler Lupo Lupo)
  }
makeLenses ''Lupo

type LupoHandler = Handler Lupo Lupo

instance SH.HasHeist Lupo where
  heistLens = subSnaplet heist

instance (MonadState Lupo m, Applicative m, Functor m) => GetLupoConfig m where
  getLupoConfig = gets lupoConfig

instance (MonadState Lupo m, Applicative m, Functor m) => L.HasLocalizer m where
  refLocalizer = gets localizer

instance (MonadState Lupo m, Applicative m, Functor m) => U.HasURLMapper m where
  getURLMapper = gets urlMapper

getNoticeDB :: MonadState Lupo m => m (N.NoticeDB LupoHandler)
getNoticeDB = gets noticeDB

withEntryDB :: (MonadCatchIO m, MonadState Lupo m) => (E.EDBWrapper -> m a) -> m a
withEntryDB handler = do
  pool <- gets entryDBPool
  C.withConnection pool handler

renderView :: Getter (V.ViewFactory LupoHandler) (V.View LupoHandler) -> LupoHandler ()
renderView v = do
  vf <- gets viewFactory
  V.render $ vf ^. v
