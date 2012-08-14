import Test_EntryDB
import Test_Syntax
import Test.Framework

main :: IO ()
main = defaultMain [dbTest, syntaxTest]
