{-# LANGUAGE ViewPatterns #-}
module Lupo.Navigation
  ( Navigation (..)
  , makeNavigation
  ) where

import Control.Applicative
import Data.Enumerator
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time

import qualified Lupo.Database as LDB
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

makeNavigation :: (Functor m, Applicative m, Monad m)
               => LDB.Database m -> Time.Day -> Navigation m
makeNavigation db base = Navigation
  { getNextDay = do
      enum <- daysAfterTommorow
      run_ $ enum $$ EL.head

  , getPreviousDay = do
      enum <- daysBeforeYesterday
      run_ $ enum $$ EL.head

  , getThisMonth =
      case Time.toGregorian base of
        (y, m, _) -> Time.fromGregorian y m 1

  , getNextPageTop = \nDays -> do
      enum <- daysAfterTommorow
      nextDays <- run_ $ enum $$ EL.take nDays
      pure $ nextDays `safeIndex` (fromIntegral $ pred nDays)

  , getPreviousPageTop = \nDays -> do
      enum <- daysBeforeYesterday
      previousDays <- run_ $ enum $$ EL.take nDays
      pure $ previousDays `safeIndex` (fromIntegral $ pred nDays)

  , getNextMonth = do
      days <- daysAfterTommorow
      e <- run_ $ days $$ findOtherMonthEntries
      pure $ pure getNextMonth' <* e

  , getPreviousMonth = do
      days <- daysBeforeYesterday
      e <- run_ $ days $$ findOtherMonthEntries
      pure $ pure getPreviousMonth' <* e
  }
  where
    daysBeforeYesterday = LDB.beforeSavedDays db yesterday
      where
        yesterday = Time.addDays (-1) base

    daysAfterTommorow = LDB.afterSavedDays db tommorow
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
