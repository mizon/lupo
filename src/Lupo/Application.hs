{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Lupo.Application
  ( LupoHandler
  , Lupo (..)
  , heist
  , session
  , auth
  , lupoConfig
  , localizer
  , noticeDB
  , urlMapper
  , entryDBPool
  , viewFactory
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

type LupoHandler = Handler Lupo Lupo

data Lupo = Lupo
  { _heist :: Snaplet (SH.Heist Lupo)
  , _session :: Snaplet S.SessionManager
  , _auth :: Snaplet (A.AuthManager Lupo)
  , _lupoConfig :: LupoConfig
  , _localizer :: L.Localizer
  , _noticeDB :: N.NoticeDB LupoHandler
  , _urlMapper :: U.URLMapper
  , _entryDBPool :: C.ConnectionPool E.EDBWrapper
  , _viewFactory :: V.ViewFactory LupoHandler
  }
makeLenses ''Lupo

instance SH.HasHeist Lupo where
  heistLens = subSnaplet heist

instance (MonadState Lupo m, Applicative m, Functor m) => GetLupoConfig m where
  getLupoConfig = use lupoConfig

instance (MonadState Lupo m, Applicative m, Functor m) => L.HasLocalizer m where
  refLocalizer = use localizer

instance (MonadState Lupo m, Applicative m, Functor m) => U.HasURLMapper m where
  getURLMapper = use urlMapper

getNoticeDB :: MonadState Lupo m => m (N.NoticeDB LupoHandler)
getNoticeDB = use noticeDB

withEntryDB :: (MonadCatchIO m, MonadState Lupo m) => (E.EDBWrapper -> m a) -> m a
withEntryDB handler = do
  pool <- use entryDBPool
  C.withConnection pool handler

renderView :: Getter (V.ViewFactory LupoHandler) (V.View LupoHandler) -> LupoHandler ()
renderView v = V.render =<< use (viewFactory . v)
