module Main (
    main
  , fast
  ) where

import Data.Monoid
import Prelude hiding (all)
import Test.Framework

import qualified Lupo.Test.Database as D
import Lupo.Test.Navigation
import Lupo.Test.Notice
import Lupo.Test.Syntax
import Lupo.Test.Util

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
