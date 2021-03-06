module Lupo.ConnectionPool
  ( ConnectionPool (..)
  , withConnection
  , makeConnectionPool
  ) where

import qualified Control.Concurrent.Chan as CC
import Control.Monad.CatchIO
import Control.Monad.Trans

import Lupo.Import

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
  pool <- initPool
  nConns `replicateM_` (checkinConnection pool =<< builder)
  pure pool
  where
    initPool = do
      chan <- CC.newChan
      pure ConnectionPool
        { checkinConnection = CC.writeChan chan
        , checkoutConnection = CC.readChan chan
        }
