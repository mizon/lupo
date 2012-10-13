module Lupo.Test.Navigation
    ( navigationTest
    ) where

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.Time as Time
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import qualified Lupo.Database as DB
import qualified Lupo.Navigation as N

mockedDB :: DB.Database
mockedDB = DB.Database
    { select = \i ->
    ,
    }

navigationTest :: Test
navigationTest = testGroup "page navigation"
    [ testCase "getNextDay" $ do
        let n = N.initNavigation
    ]
