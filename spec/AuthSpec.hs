{-# OPTIONS_GHC -fno-warn-missing-fields #-}

module AuthSpec
  ( authSpec
  ) where

import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Word
import Snap hiding (get)
import qualified Snap.Snaplet.Session as S
import qualified Snap.Snaplet.Session.Backends.CookieSession as Cookie
import Snap.Snaplet.Test as SST
import Snap.Test
import Test.Hspec
import Text.Printf

import qualified Lupo.Backends.Auth as A
import Lupo.Config
import Lupo.Exception
import Lupo.Import

data TestEnv = TestEnv
  { _session :: Snaplet S.SessionManager
  , _auth :: Snaplet (A.Authenticator TestEnv)
  }
makeLenses ''TestEnv

initHandler :: SnapletInit TestEnv TestEnv
initHandler = makeSnaplet "TestEnv" "For below test" Nothing $ do
  sess <- nestSnaplet "session" session $ Cookie.initCookieSessionManager "test_auth.txt" "sess" $ Just 3600
  auth' <- nestSnaplet "auth" auth $ A.initAuthenticator sess LupoConfig
    { _lcHashedPassword = hashText "foo"
    }
  return $ TestEnv sess auth'

evalAuthHandler :: MonadIO m => Handler TestEnv (A.Authenticator TestEnv) a -> m (Either T.Text a)
evalAuthHandler h = SST.evalHandler (get "/" mempty) (with auth h) initHandler

authSpec :: Spec
authSpec = describe "challenge-response authentication" $ do
  it "makes challenge strings and keeps it" $ do
    Right (maked, kept) <- evalAuthHandler $ do
      maked' <- A.prepareChallenge
      kept' <- with A.session $ S.getFromSession keyChallenge
      return (maked', kept')
    T.length maked `shouldBe` 40
    kept `shouldBe` Just maked

  it "checks if administor is logged-in or not" $ do
    evalAuthHandler A.isLoggedIn `shouldReturn` Right False
    let h = do
          with A.session $ S.setInSession "isLoggedIn" ""
          A.isLoggedIn
    evalAuthHandler h `shouldReturn` Right True

  it "succeeds to log in with a valid password" $ do
    let h = doLogin *> A.isLoggedIn
    evalAuthHandler h `shouldReturn` Right True

  it "fails to log in with a invalid password" $ do
    let h = do
           void $ A.prepareChallenge
           A.login "foo"
    evalAuthHandler h `shouldThrow` \(_ :: LoginFailed) ->
      True

  it "removes prepared challenge after logged in" $ do
    let h = do
          challenge' <- A.prepareChallenge
          let pw = hashText $ challenge' <> hashText "foo"
          before'' <- with A.session $ S.getFromSession keyChallenge
          A.login pw
          after' <- with A.session $ S.getFromSession keyChallenge
          return (challenge', before'', after')
    Right (challenge, before', after) <- evalAuthHandler h
    (challenge, before', after) `shouldBe` (challenge, Just challenge, Nothing)

  it "logs out" $ do
    evalAuthHandler (doLogin *> A.logout *> A.isLoggedIn) `shouldReturn` Right False
  where
    keyChallenge = "challenge"

doLogin :: Handler b (A.Authenticator b) ()
doLogin = do
  challenge <- A.prepareChallenge
  let pw = hashText $ challenge <> hashText "foo"
  A.login pw

hashText :: T.Text -> T.Text
hashText t = toHexText $ B.unpack (SHA1.hash $ E.encodeUtf8 t)
  where
    toHexText :: [Word8] -> T.Text
    toHexText = T.pack . concatMap (printf "%02x")
