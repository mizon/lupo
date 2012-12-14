module Lupo.Config (
    LupoConfig(..)
  , GetLupoConfig(..)
  , refLupoConfig
  ) where

import qualified Data.ByteString as BS
import Control.Applicative
import qualified Data.Text as T
import Text.XmlHtml

data LupoConfig = LupoConfig {
    lcSiteTitle :: T.Text
  , lcSqlitePath :: FilePath
  , lcLocaleFile :: FilePath
  , lcDaysPerPage :: Integer
  , lcFooterBody :: [Node]
  , lcBasePath :: BS.ByteString
  } deriving Show

class (Monad m, Applicative m, Functor m) => GetLupoConfig m where
  getLupoConfig :: m LupoConfig

refLupoConfig :: GetLupoConfig m => (LupoConfig -> a) -> m a
refLupoConfig f = f <$> getLupoConfig
