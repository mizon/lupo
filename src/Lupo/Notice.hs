module Lupo.Notice
  ( NoticeDB (..)
  , SessionBackend (..)
  ) where

import qualified Data.Text as T

data NoticeDB m = NoticeDB
  { addNotice :: T.Text -> m ()
  , popAllNotice :: m [T.Text]
  }

data SessionBackend m = SessionBackend
  { getCsrfToken :: m T.Text
  , commitSession :: m ()
  }
