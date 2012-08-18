{-# LANGUAGE TemplateHaskell
    , FlexibleInstances #-}
module Lupo.Application
    ( Lupo(Lupo, entryDB)
    , heist
    ) where

import qualified Lupo.EntryDB as EDB
import qualified Snap.Snaplet.Heist as H
import Snap
import Prelude hiding (filter)

data Lupo = Lupo
    { _heist :: Snaplet (H.Heist Lupo)
    , entryDB :: EDB.EntryDB
    }
makeLens ''Lupo

instance H.HasHeist Lupo where
    heistLens = subSnaplet heist

instance EDB.MonadEntryDB (Handler Lupo Lupo) where
    getEntryDB = gets entryDB
