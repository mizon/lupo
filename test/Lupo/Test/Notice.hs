{-# LANGUAGE OverloadedStrings #-}
module Lupo.Test.Notice (
    noticeTest
  ) where

import Control.Applicative
import Control.Monad.CatchIO
import Control.Monad.Writer
import qualified Data.Text as T
import qualified Database.HDBC as DB
import qualified Database.HDBC.Sqlite3 as Sqlite3
import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import qualified Lupo.Notice as N

noticeTest :: Test
noticeTest = testGroup "check notice db" [
    testCase "check if addNotice calls commitSession" $ do
      (_, as) <- withNoticeDB (makeSessionMock "abc") True $ flip N.addNotice "hello abc"
      as @?= [GetCsrfToken, CommitSession]

  , testCase "add notice and pop all notice" $ do
      void $ withNoticeDB (makeSessionMock "abc") False $ \ndb -> do
        N.addNotice ndb "hello abc"
        N.addNotice ndb "bye abc"
      void $ withNoticeDB (makeSessionMock "cde") False $ \ndb -> do
        N.addNotice ndb "hello cde"
        N.addNotice ndb "bye cde"

      (noticeForAbc, _) <- withNoticeDB (makeSessionMock "abc") False $ N.popAllNotice
      (noticeForCde, _) <- withNoticeDB (makeSessionMock "cde") True $ N.popAllNotice

      noticeForAbc @?= ["hello abc", "bye abc"]
      noticeForCde @?= ["hello cde", "bye cde"]

  , testCase "must delete all notice after fetching notice" $ do
      ((notice, afterPoped), _) <- withNoticeDB (makeSessionMock "abc") True $ \ndb -> do
        N.addNotice ndb "hello abc"
        N.addNotice ndb "bye abc"
        notice' <- N.popAllNotice ndb
        afterPoped' <- N.popAllNotice ndb
        pure (notice', afterPoped')
      notice @?= ["hello abc", "bye abc"]
      afterPoped @?= []
  ]


type TestM w = WriterT w IO

data SessionAction = GetCsrfToken | CommitSession
  deriving (Eq, Show)

makeSessionMock :: T.Text -> N.SessionBackend (TestM [SessionAction])
makeSessionMock token = N.SessionBackend {
    N.getCsrfToken=tell [GetCsrfToken] >> pure token
  , N.commitSession=tell [CommitSession]
  }

withNoticeDB :: Monoid w
             => N.SessionBackend (TestM w) -> Bool -> (N.NoticeDB (TestM w) -> TestM w a) -> IO (a, w)
withNoticeDB ss delete handle = runWriterT $
  bracket initialize finalize $ \conn ->
    handle $ N.makeNoticeDB conn ss
  where
    initialize = liftIO $ Sqlite3.connectSqlite3 "./test.sqlite3"

    finalize conn = when delete $ liftIO $ do
      void $ DB.run conn "DELETE FROM notice" []
      void $ DB.commit conn
      DB.disconnect conn
