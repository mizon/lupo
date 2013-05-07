{-# LANGUAGE ViewPatterns #-}

module Lupo.Navigation
  ( Navigation (..)
  , makeNavigation
  ) where

import Control.Applicative
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time

import qualified Lupo.Entry as E
import Lupo.Util

data Navigation m = Navigation
  { getNextDay :: m (Maybe Time.Day)
  , getPreviousDay :: m (Maybe Time.Day)
  , getThisMonth :: Time.Day
  , getNextPageTop :: Integer -> m (Maybe Time.Day)
  , getPreviousPageTop :: Integer -> m (Maybe Time.Day)
  , getNextMonth :: m (Maybe Time.Day)
  , getPreviousMonth :: m (Maybe Time.Day)
  }

makeNavigation :: (Functor m, Applicative m, Monad m) => E.EntryDatabase m -> Time.Day -> Navigation m
makeNavigation db base = Navigation
  { getNextDay = run_ $ daysAfterTommorow $$ EL.head
  , getPreviousDay = run_ $ daysBeforeYesterday $$ EL.head

  , getThisMonth =
      case Time.toGregorian base of
        (y, m, _) -> Time.fromGregorian y m 1

  , getNextPageTop = \nDays -> do
      nextDays <- run_ $ daysAfterTommorow $$ EL.take nDays
      pure $ nextDays `safeIndex` (fromIntegral $ pred nDays)

  , getPreviousPageTop = \nDays -> do
      previousDays <- run_ $ daysBeforeYesterday $$ EL.take nDays
      pure $ previousDays `safeIndex` (fromIntegral $ pred nDays)

  , getNextMonth = do
      e <- run_ $ daysAfterTommorow $$ findOtherMonthEntries
      pure $ pure getNextMonth' <* e

  , getPreviousMonth = do
      e <- run_ $ daysBeforeYesterday $$ findOtherMonthEntries
      pure $ pure getPreviousMonth' <* e
  }
  where
    daysBeforeYesterday = E.beforeSavedDays db yesterday
      where
        yesterday = Time.addDays (-1) base

    daysAfterTommorow = E.afterSavedDays db tommorow
      where
        tommorow = Time.addDays 1 base

    findOtherMonthEntries = EL.filter notBaseMonth =$ EL.head
      where
        notBaseMonth (Time.toGregorian -> (y, m, _)) =
          case Time.toGregorian base of
            (y', m', _) -> y /= y' || m /= m'

    getNextMonth' =
      case Time.toGregorian base of
        (y, 12, _) -> Time.fromGregorian (succ y) 1 1
        (y, m, _) -> Time.fromGregorian y (succ m) 1

    getPreviousMonth' =
      case Time.toGregorian base of
        (y, 1, _) -> Time.fromGregorian (pred y) 12 1
        (y, m, _) -> Time.fromGregorian y (pred m) 1
