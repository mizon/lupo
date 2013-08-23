module Main where

import Control.Exception
import qualified Data.Text.IO as IO
import Snap
import Snap.Snaplet.Config
import System.IO

#ifdef DEVELOPMENT
import Snap.Loader.Dynamic
#else
import Snap.Loader.Static
#endif

import Config
import qualified Lupo.Site as Site

main :: IO ()
main = do
  (conf, site, cleanup) <- $(loadSnapTH [|getConf|] 'getActions ["snaplets/heist/templates"])
  _ <- try $ httpServe conf site :: IO (Either SomeException ())
  cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
  (msgs, site, cleanup) <- runSnaplet (appEnvironment =<< getOther conf) $ Site.lupoInit lupoConfig
  IO.hPutStrLn stderr msgs
  return (site, cleanup)
