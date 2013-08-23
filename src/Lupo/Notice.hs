module Lupo.Notice where

import qualified Data.Text as T

import Lupo.Import

data NoticeDB m = NoticeDB
  { _addNotice :: T.Text -> m ()
  , _popAllNotice :: m [T.Text]
  }

addNotice :: T.Text -> Action m (NoticeDB m) ()
addNotice t = act $ \self ->
  _addNotice self t

popAllNotice :: Action m (NoticeDB m) [T.Text]
popAllNotice = act _popAllNotice

data SessionBackend m = SessionBackend
  { getCsrfToken :: m T.Text
  , commitSession :: m ()
  }
