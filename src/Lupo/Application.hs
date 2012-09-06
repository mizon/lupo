{-# LANGUAGE TemplateHaskell
    , FlexibleInstances
    , TypeSynonymInstances #-}
module Lupo.Application
    ( Lupo(Lupo, entryDB)
    , LupoHandler
    , heist
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Lupo.Locale as L
import Lupo.Config
import qualified Snap.Snaplet.Heist as SH
import qualified Text.Templating.Heist as H
import Snap
import Prelude hiding (filter)

data Lupo = Lupo
    { _heist :: Snaplet (SH.Heist Lupo)
    , entryDB :: EDB.EntryDB
    , lupoConfig :: LupoConfig
    , localizer :: L.Localizer
    }
makeLens ''Lupo

type LupoHandler = Handler Lupo Lupo

instance SH.HasHeist Lupo where
    heistLens = subSnaplet heist

instance EDB.MonadEntryDB LupoHandler where
    getEntryDB = gets entryDB

instance EDB.MonadEntryDB (H.HeistT LupoHandler) where
    getEntryDB = gets entryDB

instance GetLupoConfig LupoHandler where
    getLupoConfig = gets lupoConfig

instance L.HasLocalizer LupoHandler where
    refLocalizer = gets localizer

instance L.HasLocalizer (H.HeistT LupoHandler) where
    refLocalizer = gets localizer
