module Lupo.Locale
    ( Localizer
    , HasLocalizer(..)
    , localize
    , loadYamlLocalizer
    ) where

import Control.Applicative
import Control.Exception
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Yaml as Y

import Lupo.Exception

type Localizer = T.Text -> Maybe T.Text

class (Monad m, Applicative m, Functor m) => HasLocalizer m where
    refLocalizer :: m Localizer

localize :: HasLocalizer m => T.Text -> m T.Text
localize t = fromMaybe t <$> (refLocalizer <*> pure t)

loadYamlLocalizer :: FilePath -> IO Localizer
loadYamlLocalizer f = Y.decodeFile f >>=
    maybe (throw InvalidLocaleFile) (\t -> pure $ flip M.lookup t)
