import Control.Monad
import Test.Hspec

import DatabaseSpec
import NavigationSpec
import NoticeSpec
import UtilSpec

main :: IO ()
main = hspec $ sequence_ [
    databaseSpec
  , savedObjectSpec
  , navigationSpec
  , noticeSpec
  , utilSpec
  ]
