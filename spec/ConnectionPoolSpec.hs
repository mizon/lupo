module ConnectionPoolSpec
  ( connectionPoolSpec
  ) where

import Control.Applicative
import Test.Hspec

import Lupo.ConnectionPool

connectionPoolSpec :: Spec
connectionPoolSpec = describe "connection pool" $ do
  it "pools some connections" $ do
    pool <- makeConnectionPool (pure "connection") 10
    withConnection pool $ \conn ->
      conn `shouldBe` "connection"
