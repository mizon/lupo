module Lupo.Config
  ( LupoConfig (..)
  , GetLupoConfig (..)
  , refLupoConfig
  ) where

import qualified Data.ByteString as BS
import Control.Applicative
import qualified Data.Text as T
import Text.XmlHtml

import qualified Lupo.Entry as E

data LupoConfig = LupoConfig
  { lcSiteTitle :: T.Text
  , lcSqlitePath :: FilePath
  , lcLanguage :: T.Text
  , lcLocaleFile :: FilePath
  , lcDaysPerPage :: Integer
  , lcFooterBody :: [Node]
  , lcBasePath :: BS.ByteString
  , lcSpamFilter :: E.Comment -> Bool
  }

class (Monad m, Applicative m, Functor m) => GetLupoConfig m where
  getLupoConfig :: m LupoConfig

refLupoConfig :: GetLupoConfig m => (LupoConfig -> a) -> m a
refLupoConfig f = f <$> getLupoConfig
