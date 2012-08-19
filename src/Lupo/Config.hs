module Lupo.Config
    ( LupoConfig(..)
    , GetLupoConfig(..)
    , refLupoConfig
    ) where

import qualified Data.Text as T
import Control.Applicative

data LupoConfig = LupoConfig
    { lcSiteTitle :: T.Text
    , lcSqlitePath :: FilePath
    } deriving Show

class (Monad m, Applicative m, Functor m) => GetLupoConfig m where
    getLupoConfig :: m LupoConfig

refLupoConfig :: GetLupoConfig m => (LupoConfig -> a) -> m a
refLupoConfig f = f <$> getLupoConfig
