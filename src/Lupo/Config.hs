{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lupo.Config
  ( LupoConfig (..)
  , lcSiteTitle
  , lcSqlitePath
  , lcLanguage
  , lcLocaleFile
  , lcDaysPerPage
  , lcFooterBody
  , lcBasePath
  , lcSpamFilter
  , lcAuthorName
  , GetLupoConfig (..)
  , refLupoConfig
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as T
import Text.XmlHtml

import qualified Lupo.Entry as E
import Lupo.Import

data LupoConfig = LupoConfig
  { _lcSiteTitle :: T.Text
  , _lcSqlitePath :: FilePath
  , _lcLanguage :: T.Text
  , _lcLocaleFile :: FilePath
  , _lcDaysPerPage :: Integer
  , _lcFooterBody :: [Node]
  , _lcBasePath :: BS.ByteString
  , _lcSpamFilter :: E.Comment -> Bool
  , _lcAuthorName :: T.Text
  }
makeLenses ''LupoConfig

class (Monad m, Applicative m, Functor m) => GetLupoConfig m where
  getLupoConfig :: m LupoConfig

refLupoConfig :: GetLupoConfig m => Getter LupoConfig a -> m a
refLupoConfig getter = getLupoConfig <&> view getter
