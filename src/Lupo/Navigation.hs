module Lupo.Navigation
  ( Navigation (..)
  ) where

import qualified Data.Time as Time

data Navigation m = Navigation
  { getNextDay :: m (Maybe Time.Day)
  , getPreviousDay :: m (Maybe Time.Day)
  , getThisMonth :: Time.Day
  , getNextPageTop :: Integer -> m (Maybe Time.Day)
  , getPreviousPageTop :: Integer -> m (Maybe Time.Day)
  , getNextMonth :: m (Maybe Time.Day)
  , getPreviousMonth :: m (Maybe Time.Day)
  }
