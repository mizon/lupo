{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Lupo.Application
    ( Lupo(Lupo, entryDB)
    , LupoHandler
    , heist
    ) where

import Control.Monad.CatchIO hiding (Handler)
import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Heist as SH

import Lupo.Config
import qualified Lupo.Database as LDB
import qualified Lupo.Locale as L

data Lupo = Lupo
    { _heist :: Snaplet (SH.Heist Lupo)
    , entryDB :: LDB.Database
    , lupoConfig :: LupoConfig
    , localizer :: L.Localizer
    }
makeLens ''Lupo

type LupoHandler = Handler Lupo Lupo

instance SH.HasHeist Lupo where
    heistLens = subSnaplet heist

instance MonadState Lupo m => LDB.HasDatabase m where
    getDatabase = gets entryDB

instance (MonadCatchIO m, Applicative m, Functor m) => LDB.DatabaseContext m

instance (MonadState Lupo m, Applicative m, Functor m) => GetLupoConfig m where
    getLupoConfig = gets lupoConfig

instance (MonadState Lupo m, Applicative m, Functor m) => L.HasLocalizer m where
    refLocalizer = gets localizer
