import Control.Monad
import Test.Hspec

import AuthSpec
import ConnectionPoolSpec
import EntrySpec
import NavigationSpec
import NoticeSpec
import SyntaxSpec
import URLMapperSpec
import UtilSpec

main :: IO ()
main = hspec $ sequence_
  [ entrySpec
  , savedObjectSpec
  , navigationSpec
  , noticeSpec
  , utilSpec
  , syntaxSpec
  , urlMapperSpec
  , connectionPoolSpec
  , authSpec
  ]
