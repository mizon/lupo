module Lupo.Navigation
  ( Navigation (..)
  , getNextDay
  , getPreviousDay
  , getThisMonth
  , getNextPageTop
  , getPreviousPageTop
  , getNextMonth
  , getPreviousMonth
  ) where

import qualified Data.Time as Time

import Lupo.Import

data Navigation m = Navigation
  { _getNextDay :: m (Maybe Time.Day)
  , _getPreviousDay :: m (Maybe Time.Day)
  , _getThisMonth :: Time.Day
  , _getNextPageTop :: Integer -> m (Maybe Time.Day)
  , _getPreviousPageTop :: Integer -> m (Maybe Time.Day)
  , _getNextMonth :: m (Maybe Time.Day)
  , _getPreviousMonth :: m (Maybe Time.Day)
  }

getNextDay :: Action m (Navigation m) (Maybe Time.Day)
getNextDay = act _getNextDay

getPreviousDay :: Action m (Navigation m) (Maybe Time.Day)
getPreviousDay = act _getPreviousDay

getThisMonth :: Getter (Navigation m) Time.Day
getThisMonth = to _getThisMonth

getNextPageTop :: Integer -> Action m (Navigation m) (Maybe Time.Day)
getNextPageTop n = act $ \self ->
  _getNextPageTop self n

getPreviousPageTop :: Integer -> Action m (Navigation m) (Maybe Time.Day)
getPreviousPageTop n = act $ \self ->
  _getPreviousPageTop self n

getNextMonth :: Action m (Navigation m) (Maybe Time.Day)
getNextMonth = act _getNextMonth

getPreviousMonth :: Action m (Navigation m) (Maybe Time.Day)
getPreviousMonth = act _getPreviousMonth
