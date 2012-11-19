{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
module Lupo.FieldValidator (
    FieldValidator(..)
  , makeFieldValidator
  , checkIsEmtpy
  , checkIsTooLong
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Writer
import qualified Data.Text as T
import Text.Shakespeare.Text

import Lupo.Exception

tooLong :: Int
tooLong = 1024 * 1024

data FieldValidator a = FieldValidator {
    validate :: (Functor m, Applicative m, MonadCatchIO m) => a -> m ()
  }

makeFieldValidator :: (a -> Writer [T.Text] ()) -> FieldValidator a
makeFieldValidator f = FieldValidator {
    validate = \a ->
      case execWriter $ f a of
        [] -> pure ()
        messages -> throw $ InvalidField messages
  }

checkIsEmtpy :: T.Text -> T.Text -> Writer [T.Text] ()
checkIsEmtpy value fieldName =
  when (T.null $ T.strip value) $
    tell $ pure [st|#{fieldName} is empty.|]

checkIsTooLong :: T.Text -> T.Text -> Writer [T.Text] ()
checkIsTooLong value fieldName =
  when (T.length value > tooLong) $
    tell $ pure [st|#{fieldName} is too long.|]
