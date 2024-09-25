module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Complex.Cyclotomic
import Pecac.Cyclotomic

import qualified Data.Complex.Cyclotomic as Cyclotomic

-----------------------------------------------------------------------------------------
-- cycOrder

cycOrderGenTestCase n = 
    if mod n 4 == 2
    then assertEqual msg (div n 2) val
    else assertEqual msg n val
    where val = cycOrder $ Cyclotomic.e n
          msg = "Obtained " ++ show val ++ " for: cycOrder $ Cyclotomic.e " ++ show n

cycOrderGenTest 1 = cycOrderGenTestCase 1
cycOrderGenTest n = do
    cycOrderGenTestCase n
    cycOrderGenTest $ n - 1

test1 = TestCase $ cycOrderGenTest 1000

test2 = TestCase (assertEqual "cycOrder computes the order of a sum of roots of unity."
                              3
                              (cycOrder sum))
    where c1 = Cyclotomic.e 9 -- E(9)
          c2 = c1 * c1        -- E(9)^2
          c3 = c2 * c1        -- E(9)^3 = E(3)
          c4 = c3 * c1        -- E(9)^4
          c5 = c4 * c1        -- E(9)^5
          c6 = c5 * c1        -- E(9)^6 = E(3)^2
          c7 = c6 * c1        -- E(9)^7
          c8 = c7 * c1        -- E(9)^8
          c9 = c8 * c1        -- E(9)^9 = E(3)^3 = 1
          -- All 9 roots sum to 0, so this sums to (-c3 - c9).
          sum = c1 + c2 + c4 + c5 + c6 + c7 + c8

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "cycOrder_PrimitiveRoots" test1,
                                     TestLabel "cycOrder_SumOfRoots" test2]

main = defaultMain tests
