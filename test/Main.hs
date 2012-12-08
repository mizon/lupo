module Main (
    main
  , fast
  ) where

import Data.Monoid
import Prelude hiding (all)
import Test.Framework

import qualified DatabaseTest as D
import NavigationTest
import NoticeTest
import SyntaxTest
import UtilTest

main, fast :: IO ()
main = defaultMain $ slowTests <> fastTests
fast = defaultMain fastTests

fastTests, slowTests :: [Test]
fastTests = [
    D.savedTest
  , navigationTest
  , syntaxTest
  , utilTest
  ]
slowTests = [D.dbTest, noticeTest]
