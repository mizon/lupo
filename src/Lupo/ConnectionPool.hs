{-# LANGUAGE TemplateHaskell #-}

module Lupo.ConnectionPool
  ( ConnectionPool (..)
  , withConnection
  , makeConnectionPool
  ) where

import Control.Applicative
import qualified Control.Concurrent.Chan as CC
import Control.Monad
import Control.Monad.CatchIO
import Control.Monad.Trans

data ConnectionPool conn = ConnectionPool
  { checkoutConnection :: IO conn
  , checkinConnection :: conn -> IO ()
  }

withConnection :: MonadCatchIO m => ConnectionPool conn -> (conn -> m a) -> m a
withConnection pool = bracket checkout checkin
  where
    checkout = liftIO $ checkoutConnection pool
    checkin = liftIO . checkinConnection pool

makeConnectionPool :: IO conn -> Int -> IO (ConnectionPool conn)
makeConnectionPool builder nConns = do
  chan <- CC.newChan
  nConns `replicateM_` (CC.writeChan chan =<< builder)
  pure ConnectionPool
    { checkinConnection = CC.writeChan chan
    , checkoutConnection = CC.readChan chan
    }
