import Control.Exception
import qualified Data.Text.IO as IO
import Snap
import Snap.Snaplet.Config
import System.IO
import Text.XmlHtml

#ifdef DEVELOPMENT
import Snap.Loader.Dynamic
#else
import Snap.Loader.Static
#endif

import Lupo.Config
import qualified Lupo.Site as Site

lupoConf :: LupoConfig
lupoConf = LupoConfig
  { _lcSiteTitle = "Lupo Web Diary"
  , _lcSqlitePath = "./development.sqlite3"
  , _lcLanguage = "ja"
  , _lcLocaleFile = "./ja.yml"
  , _lcDaysPerPage = 5
  , _lcFooterBody =
    [ Element "p" []
      [ TextNode "Powered by "
      , Element "a" [("href", "http://www.haskell.org/haskellwiki/Haskell")] [TextNode "Haskell"]
      , TextNode ", "
      , Element "a" [("href", "http://snapframework.com/")] [TextNode "Snap Framework"]
      ]
    ]
  , _lcBasePath = "http://localhost:8000"
  , _lcSpamFilter = const True
  , _lcAuthorName = ""
  }

main :: IO ()
main = do
  (conf, site, cleanup) <- $(loadSnapTH [|getConf|] 'getActions ["snaplets/heist/templates"])
  _ <- try $ httpServe conf site :: IO (Either SomeException ())
  cleanup

getConf :: IO (Config Snap AppConfig)
getConf = commandLineAppConfig defaultConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
  (msgs, site, cleanup) <- runSnaplet (appEnvironment =<< getOther conf) $ Site.lupoInit lupoConf
  IO.hPutStrLn stderr msgs
  return (site, cleanup)
