module Lupo.Entry where

import Control.Monad.CatchIO
import qualified Data.Enumerator as E
import qualified Data.Text as T
import qualified Data.Time as Time
import Prelude hiding (all)

import Lupo.Import
import Lupo.Util

data Saved o = Saved
  { _idx :: Integer
  , _createdAt :: Time.ZonedTime
  , _modifiedAt :: Time.ZonedTime
  , _savedContent :: o
  } deriving Show
makeLenses ''Saved

instance Eq o => Eq (Saved o) where
  self == other = self ^. idx == other ^. idx && self ^. savedContent == other ^. savedContent

data Entry = Entry
  { _entryTitle :: T.Text
  , _entryBody :: T.Text
  } deriving (Eq , Show)
makeLenses ''Entry

data Comment = Comment
  { _commentName :: T.Text
  , _commentBody :: T.Text
  } deriving (Eq, Show)
makeLenses ''Comment

data Page = Page
  { _pageDay :: Time.Day
  , _pageEntries :: [Saved Entry]
  , _numOfComments :: Int
  , _pageComments :: [Saved Comment]
  } deriving (Eq, Show)
makeLenses ''Page

getCreatedDay :: Getter (Saved a) Time.Day
getCreatedDay = createdAt . to zonedDay

data EntryDatabase m = EntryDatabase
  { _selectOne :: Integer -> m (Saved Entry)
  , _selectAll :: forall a. E.Enumerator (Saved Entry) m a
  , _selectPage :: Time.Day -> m Page
  , _search :: T.Text -> forall a. E.Enumerator (Saved Entry) m a
  , _insert :: Entry -> m ()
  , _update :: Integer -> Entry -> m ()
  , _delete :: Integer -> m ()
  , _beforeSavedDays :: forall a. Time.Day -> E.Enumerator Time.Day m a
  , _afterSavedDays :: forall a. Time.Day -> E.Enumerator Time.Day m a
  , _insertComment :: Time.Day -> Comment -> m ()
  }

selectOne :: Integer -> Action m (EntryDatabase m) (Saved Entry)
selectOne i = act $ \self ->
  _selectOne self i

selectAll :: Getter (EntryDatabase m) (E.Enumerator (Saved Entry) m a)
selectAll = to _selectAll

selectPage :: Time.Day -> Action m (EntryDatabase m) Page
selectPage d = act $ \self ->
  _selectPage self d

search :: T.Text -> Getter (EntryDatabase m) (E.Enumerator (Saved Entry) m a)
search t = to $ \self ->
  _search self t

insert :: Entry -> Action m (EntryDatabase m) ()
insert e = act $ \self ->
  _insert self e

update :: Integer -> Entry -> Action m (EntryDatabase m) ()
update i e = act $ \self ->
  _update self i e

delete :: Integer -> Action m (EntryDatabase m) ()
delete i = act $ \self ->
  _delete self i

beforeSavedDays :: Time.Day -> Getter (EntryDatabase m) (E.Enumerator Time.Day m a)
beforeSavedDays d = to $ \self ->
  _beforeSavedDays self d

afterSavedDays :: Time.Day -> Getter (EntryDatabase m) (E.Enumerator Time.Day m a)
afterSavedDays d = to $ \self ->
  _afterSavedDays self d

insertComment :: Time.Day -> Comment -> Action m (EntryDatabase m) ()
insertComment d c = act $ \self ->
  _insertComment self d c

data EDBWrapper = EDBWrapper
  { unEDBWrapper :: (MonadCatchIO m, Applicative m, Functor m) => EntryDatabase m
  }
