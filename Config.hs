module Config
  ( lupoConfig
  ) where

import Text.XmlHtml

import Lupo.Config

lupoConfig :: LupoConfig
lupoConfig = LupoConfig
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
