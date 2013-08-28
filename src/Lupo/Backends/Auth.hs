module Lupo.Backends.Auth
  ( module Lupo.Auth
  , initAuthenticator
  ) where

import Control.Error
import Control.Monad.CatchIO
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import Data.Char
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word
import Snap
import qualified Snap.Snaplet.Session as S
import qualified System.Random as Random
import Text.Printf

import Lupo.Auth
import Lupo.Config
import Lupo.Import

keyChallenge, keyIsLoggedIn :: T.Text
keyChallenge = "challenge"
keyIsLoggedIn = "isLoggedIn"

initAuthenticator :: Snaplet S.SessionManager -> LupoConfig -> SnapletInit b (Authenticator b)
initAuthenticator sess lc = makeSnaplet "Authenticator" "Provide challenge-response authentications" Nothing $ do
  return authenticator
  where
    authenticator = Authenticator
      { _prepareChallenge = do
          challenge <- liftIO getRandomText
          with session $ S.setInSession keyChallenge challenge
          return challenge

      , _isLoggedIn = isJust <$> with session (S.getFromSession keyIsLoggedIn)

      , _login = \pw -> do
         with session $ S.getFromSession keyChallenge >>= \case
           Just challenge -> do
             S.deleteFromSession keyChallenge
             let valid = hashText $ challenge <> lc ^. lcHashedPassword
             if pw == valid then
               S.setInSession keyIsLoggedIn ""
             else
               throw LoginFailed
           Nothing -> throw LoginFailed

      , _logout = with session $ S.deleteFromSession keyIsLoggedIn
      , _session = sess
      }

hashText :: T.Text -> T.Text
hashText t = toHexText $ B.unpack (SHA1.hash $ E.encodeUtf8 t)

getRandomText :: IO T.Text
getRandomText = do
  gen <- Random.newStdGen
  return $ toHexText $ take 20 $ fromIntegral <$> Random.randomRs (0, 255 :: Int) gen

toHexText :: [Word8] -> T.Text
toHexText = T.pack . concatMap (printf "%02x")
