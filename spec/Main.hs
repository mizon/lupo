import Control.Monad
import Test.Hspec

import DatabaseSpec
import NavigationSpec
import NoticeSpec
import SyntaxSpec
import URLMapperSpec
import UtilSpec

main :: IO ()
main = hspec $ sequence_
  [ databaseSpec
  , savedObjectSpec
  , navigationSpec
  , noticeSpec
  , utilSpec
  , syntaxSpec
  , urlMapperSpec
  ]
