{-# LANGUAGE ViewPatterns #-}
module Lupo.Navigation (
    Navigation(..)
  , makeNavigation
  ) where

import Control.Applicative
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time

import qualified Lupo.Database as LDB
import Lupo.Util

data Navigation m = Navigation {
    getNextDay :: m (Maybe Time.Day)
  , getPreviousDay :: m (Maybe Time.Day)
  , getThisMonth :: Time.Day
  , getNextPageTop :: Integer -> m (Maybe Time.Day)
  , getPreviousPageTop :: Integer -> m (Maybe Time.Day)
  , getNextMonth :: m Time.Day
  , getPreviousMonth :: m Time.Day
  }

makeNavigation :: (Functor m, Applicative m, Monad m) =>
  LDB.Database m -> Time.Day -> Navigation m
makeNavigation db base = Navigation {
    getNextDay = run_ =<< (EL.head >>==) <$> daysAfterTommorow
  , getPreviousDay = run_ =<< (EL.head >>==) <$> daysBeforeYesterday

  , getThisMonth =
      case Time.toGregorian base of
        (y, m, _) -> Time.fromGregorian y m 1

  , getNextPageTop = \nDays -> do
      nextDays <- run_ =<< (EL.take nDays >>==) <$> daysAfterTommorow
      pure $ safeLast nextDays

  , getPreviousPageTop = \nDays -> do
      previousDays <- run_ =<< (EL.take nDays >>==) <$> daysBeforeYesterday
      pure $ safeLast previousDays

  , getNextMonth = pure $
      case Time.toGregorian base of
        (y, 12, _) -> Time.fromGregorian (y + 1) 1 1
        (y, m, _) -> Time.fromGregorian y (m + 1) 1

  , getPreviousMonth = pure $
      case Time.toGregorian base of
        (y, 1, _) -> Time.fromGregorian (y - 1) 12 1
        (y, m, _) -> Time.fromGregorian y (m - 1) 1
  }
  where
    daysBeforeYesterday = LDB.beforeSavedDays db yesterday
      where
        yesterday = Time.addDays (-1) base

    daysAfterTommorow = LDB.afterSavedDays db tommorow
      where
        tommorow = Time.addDays 1 base
