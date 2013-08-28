module Lupo.Auth where

import Snap
import qualified Data.Text as T

data Authenticator b = Authenticator
  { _prepareChallenge :: Handler b (Authenticator b) T.Text
  , _isLoggedIn :: Handler b (Authenticator b) Bool
  , _login :: T.Text -> Handler b (Authenticator b) ()
  , _logout :: Handler b (Authenticator b) ()
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
