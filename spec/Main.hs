import Control.Monad
import Test.Hspec

import DatabaseSpec
import NavigationSpec
import NoticeSpec
import SyntaxSpec
import UtilSpec

main :: IO ()
main = hspec $ sequence_ [
    databaseSpec
  , savedObjectSpec
  , navigationSpec
  , noticeSpec
  , utilSpec
  , syntaxSpec
  ]
