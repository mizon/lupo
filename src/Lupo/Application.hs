{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Lupo.Application
  ( Lupo(Lupo, entryDB)
  , LupoHandler
  , heist
  ) where

import Prelude hiding (filter)
import Snap
import qualified Snap.Snaplet.Heist as SH

import Lupo.Config
import qualified Lupo.Database as LDB
import qualified Lupo.Locale as L

data Lupo = Lupo
  { _heist :: Snaplet (SH.Heist Lupo)
  , entryDB :: LDB.DatabaseContext m => LDB.Database m
  , lupoConfig :: LupoConfig
  , localizer :: L.Localizer
  }
makeLens ''Lupo

type LupoHandler = Handler Lupo Lupo

instance SH.HasHeist Lupo where
  heistLens = subSnaplet heist

instance (LDB.DatabaseContext m, MonadState Lupo m) => LDB.HasDatabase m where
  getDatabase = gets entryDB

instance (MonadState Lupo m, Applicative m, Functor m) => GetLupoConfig m where
  getLupoConfig = gets lupoConfig

instance (MonadState Lupo m, Applicative m, Functor m) => L.HasLocalizer m where
  refLocalizer = gets localizer
