module Lupo.Auth where

import Snap
import qualified Snap.Snaplet.Session as S
import qualified Data.Text as T

import Lupo.Import

data Authenticator b = Authenticator
  { _prepareChallenge :: Handler b (Authenticator b) T.Text
  , _isLoggedIn :: Handler b (Authenticator b) Bool
  , _login :: T.Text -> Handler b (Authenticator b) ()
  , _logout :: Handler b (Authenticator b) ()
  , _session :: Snaplet S.SessionManager
  }

prepareChallenge :: Handler b (Authenticator b) T.Text
prepareChallenge = _prepareChallenge =<< get

isLoggedIn :: Handler b (Authenticator b) Bool
isLoggedIn = _isLoggedIn =<< get

login :: T.Text -> Handler b (Authenticator b) ()
login pw = do
  auth <- get
  _login auth pw

logout :: Handler b (Authenticator b) ()
logout = _logout =<< get

session :: SnapletLens (Authenticator b) S.SessionManager
session = lens _session $ \a s ->
  a {_session = s}
