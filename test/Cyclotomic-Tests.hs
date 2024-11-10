module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Complex.Cyclotomic
import Data.Ratio
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

cycOrderGenTest 0 = pure ()
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
-- einv

einvGenTestCase n m z = assertEqual msg (Just exp) act
    where exp = if m == 0 then 0 else m % n
          act = einv z
          msg = "Obtained " ++ show act ++ " from einv instead of: " ++ show exp

einvGenExpand n m z =
    if n == m then pure () else do
        einvGenTestCase n m $ z^m
        einvGenExpand n (m + 1) z

einvGenTest 1 = einvGenTestCase 1 0 1
einvGenTest n = do
    einvGenExpand n 1 $ Cyclotomic.e n
    einvGenTest $ n - 1

test3 = TestCase $ einvGenTest 150

test4 = TestCase (assertEqual "einv handles zero."
                              Nothing
                              (einv 0))

test5 = TestCase (assertEqual "einv handles norm one elements of infinite order (1/2)."
                              Nothing
                              (einv z))
    where z = (3 - 4 * Cyclotomic.i) / 5

test6 = TestCase (assertEqual "einv handles norm one elements of infinite order (2/2)."
                              Nothing
                              (einv z))
    where sqrt7 = Cyclotomic.sqrtInteger 7
          z     = (sqrt7 * Cyclotomic.i - 3) / 4

test7 = TestCase (assertEqual "einv handles elements off the unit circle (1/2)."
                              Nothing
                              (einv z))
    where z = 2 * Cyclotomic.i + Cyclotomic.e 7

test8 = TestCase (assertEqual "einv handles elements off the unit circle (2/2)."
                              Nothing
                              (einv z))
    where e1 = Cyclotomic.e 31
          e2 = Cyclotomic.e 12
          e3 = Cyclotomic.e 8
          z  = 5 * e1 - 7 * e2 + 22 * e3

test9 = TestCase (assertEqual "einv handles a specific value."
                              (Just $ 2 % 9)
                              (einv z))
    where z = (Cyclotomic.e 9)^2

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "cycOrder_PrimitiveRoots" test1,
                                     TestLabel "cycOrder_SumOfRoots" test2,
                                     TestLabel "einv_RootsOfUnity" test3,
                                     TestLabel "einv_Zero" test4,
                                     TestLabel "einv_IrrUnit_1" test5,
                                     TestLabel "einv_IrrUnit_2" test6,
                                     TestLabel "einv_NonUnit_1" test7,
                                     TestLabel "einv_NonUnit_2" test8,
                                     TestLabel "einv_Specific" test9]

main = defaultMain tests
