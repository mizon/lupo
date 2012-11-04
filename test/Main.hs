import Test.Framework

import Lupo.Test.Database
import Lupo.Test.Navigation
import Lupo.Test.Syntax
import Lupo.Test.Util

main :: IO ()
main = defaultMain
  [ dbTest
  , savedTest
  , navigationTest
  , syntaxTest
  , utilTest
  ]
