{-# LANGUAGE OverloadedStrings #-}

module NoticeSpec
  ( noticeSpec
  ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Test.Hspec

import qualified Lupo.Backends.Notice as N
import qualified Lupo.Notice as N

noticeSpec :: Spec
noticeSpec = describe "notice database" $ do
  it "calls commitSession before saves notice message" $ do
    (_, as) <- withNoticeDB (makeSessionMock "abc") True $ flip N.addNotice "hello abc"
    as `shouldBe` [GetCsrfToken, CommitSession]

  it "adds notice and pop all notice" $ do
    void $ withNoticeDB (makeSessionMock "abc") False $ \ndb -> do
      N.addNotice ndb "hello abc"
      N.addNotice ndb "bye abc"
    void $ withNoticeDB (makeSessionMock "cde") False $ \ndb -> do
      N.addNotice ndb "hello cde"
      N.addNotice ndb "bye cde"

    (noticeForAbc, _) <- withNoticeDB (makeSessionMock "abc") False N.popAllNotice
    (noticeForCde, _) <- withNoticeDB (makeSessionMock "cde") True N.popAllNotice

    noticeForAbc `shouldBe` ["hello abc", "bye abc"]
    noticeForCde `shouldBe` ["hello cde", "bye cde"]

  it "deletes all notice after fetching notice" $ do
    ((notice, afterPoped), _) <- withNoticeDB (makeSessionMock "abc") True $ \ndb -> do
      N.addNotice ndb "hello abc"
      N.addNotice ndb "bye abc"
      notice' <- N.popAllNotice ndb
      afterPoped' <- N.popAllNotice ndb
      pure (notice', afterPoped')
    notice `shouldBe` ["hello abc", "bye abc"]
    afterPoped `shouldBe` []

type TestM w = WriterT w IO

data SessionAction = GetCsrfToken | CommitSession
  deriving (Eq, Show)

makeSessionMock :: T.Text -> N.SessionBackend (TestM [SessionAction])
makeSessionMock token = N.SessionBackend
  { N.getCsrfToken = tell [GetCsrfToken] >> pure token
  , N.commitSession = tell [CommitSession]
  }

withNoticeDB :: Monoid w
             => N.SessionBackend (TestM w)
             -> Bool
             -> (N.NoticeDB (TestM w)
             -> TestM w a)
             -> IO (a, w)
withNoticeDB ss ifCleanup handle = runWriterT $
  bracket initialize finalize $ \conn ->
    handle $ N.makeNoticeDB conn ss
  where
    initialize = liftIO $ Sqlite3.connectSqlite3 "./test.sqlite3"

    finalize conn = when ifCleanup $ liftIO $ do
      void $ DB.run conn "DELETE FROM notice" []
      void $ DB.commit conn
      DB.disconnect conn
