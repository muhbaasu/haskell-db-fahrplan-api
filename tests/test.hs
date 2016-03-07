import Test.Tasty
import Test.Tasty.HUnit

import Integration
import Quickcheck
import Unit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, quickCheckTests, integrationTests]
