import Test.Tasty
import Test.Tasty.HUnit

import Quickcheck
import Unit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, quickCheckTests]

