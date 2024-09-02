module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Ratio
import Pecac.Analyzer.Revolution

-----------------------------------------------------------------------------------------
-- Tests that asRational is the right-inverse to rationalToRev.

invRatTest angle j = TestCase (assertEqual msg angle (asRational $ rationalToRev angle))
    where msg = "asRational is the left inverse to rationalToRev (" ++ show j ++ "/6)."

test1 = invRatTest (0 % 1) 1
test2 = invRatTest (2 % 1) 2
test3 = invRatTest (1 % 2) 3
test4 = invRatTest (1 % 3) 4
test5 = invRatTest (7 % 9) 5
test6 = invRatTest (-11 % 13) 6

-----------------------------------------------------------------------------------------
-- Tests that ratioToRev agrees with rationalToRev.

ratioTest n d j = TestCase (assertEqual msg lhs rhs)
    where msg = "ratioToRev agrees with rationalToRev (" ++ show j ++ "/6)."
          lhs = ratioToRev n d
          rhs = Just $ rationalToRev $ n % d

test7  = ratioTest 0 1 1
test8  = ratioTest 2 1 2
test9  = ratioTest 1 2 3
test10 = ratioTest 1 3 4
test11 = ratioTest 7 9 5
test12 = ratioTest (-11) 13 6

-----------------------------------------------------------------------------------------
-- Tests that ratioToRev handles zero denominators.

test13 = TestCase (assertEqual "ratioToRev safely rejects zero denominators (1/2)."
                               Nothing
                               (ratioToRev 5 0))

test14 = TestCase (assertEqual "ratioToRev safely rejects zero denominators (2/2)."
                               Nothing
                               (ratioToRev (-12) 0))

-----------------------------------------------------------------------------------------
-- strToRev

test15 = TestCase (assertEqual "strToRev agrees with Parser.Revolution (1/4)."
                               (ratioToRev 2 5)
                               (strToRev "2/5"))

test16 = TestCase (assertEqual "strToRev agrees with Parser.Revolution (2/4)."
                               (ratioToRev (-7) 3)
                               (strToRev "-7/3"))

test17 = TestCase (assertEqual "strToRev agrees with Parser.Revolution (3/4)."
                               (ratioToRev 3 4)
                               (strToRev "3 / 2 * pi"))

test18 = TestCase (assertEqual "strToRev agrees with Parser.Revolution (4/4)."
                               (ratioToRev 1 1)
                               (strToRev "2 * pi"))

-----------------------------------------------------------------------------------------
-- show

invShowTest angle j = TestCase (assertEqual msg rev (fromJust $ strToRev $ show rev))
    where msg = "(fromJust . strToRev) is the left inverse to show (" ++ show j ++ "/6)."
          rev = rationalToRev angle

test19 = invShowTest (0 % 1) 1
test20 = invShowTest (2 % 1) 2
test21 = invShowTest (1 % 2) 3
test22 = invShowTest (1 % 3) 4
test23 = invShowTest (7 % 9) 5
test24 = invShowTest (-11 % 13) 6

-----------------------------------------------------------------------------------------
-- Revolution as a semigroup.

addTest x y = TestCase (assertEqual msg lhs rhs)
    where msg = "Failed to add " ++ show x ++ " with " ++ show y ++ " as revolutions."
          lhs = rationalToRev x <> rationalToRev y
          rhs = rationalToRev $ x + y

test25 = addTest (1 % 2) (1 % 3)
test26 = addTest (2 % 5) (-3 % 7)
test27 = addTest (0 % 1) (7 % 3)
test28 = addTest (20 % 21) (1 % 100)
test29 = addTest (-20 % 3) (1 % 5)
test30 = addTest (1 % 2) (1 % 2)

-----------------------------------------------------------------------------------------
-- Revolution as a monoid.

lidTest angle = TestCase (assertEqual msg lhs rhs)
    where msg = "Identity failed on the left-hand side of " ++ show angle ++ "."
          lhs = rationalToRev angle
          rhs = mempty <> rationalToRev angle

ridTest angle = TestCase (assertEqual msg lhs rhs)
    where msg = "Identity failed on the right-hand side of " ++ show angle ++ "."
          lhs = rationalToRev angle
          rhs = rationalToRev angle <> mempty

test31 = lidTest (0 % 1)
test32 = ridTest (0 % 1)
test33 = lidTest (1 % 2)
test34 = ridTest (1 % 2)
test35 = lidTest (-5 % 7)
test36 = ridTest (-5 % 7)

-----------------------------------------------------------------------------------------
-- Revolution as a group.

linvTest angle = TestCase (assertEqual msg mempty (rev <> invert rev))
    where msg = "Inversion failed on the left-hand side of " ++ show angle ++ "."
          rev = rationalToRev angle

rinvTest angle = TestCase (assertEqual msg mempty ((invert rev) <> rev))
    where msg = "Inversion failed on the right-hand side of " ++ show angle ++ "."
          rev = rationalToRev angle

test37 = linvTest (0 % 1)
test38 = rinvTest (0 % 1)
test39 = linvTest (1 % 2)
test40 = rinvTest (1 % 2)
test41 = linvTest (-5 % 7)
test42 = rinvTest (-5 % 7)

-----------------------------------------------------------------------------------------
-- scale.

scaleTest s angle = TestCase (assertEqual msg lhs rhs)
    where msg = "Failed to scale " ++ show angle ++ " by " ++ show s ++ "."
          lhs = scale s $ rationalToRev angle
          rhs = rationalToRev $ s * angle

test43 = scaleTest (1 % 2) (1 % 11)
test44 = scaleTest (1 % 2) (-3 % 101)
test45 = scaleTest (3 % 5) (1 % 11)
test46 = scaleTest (3 % 5) (-3 % 101)
test47 = scaleTest (-7 % 9) (1 % 11)
test48 = scaleTest (-7 % 9) (-3 % 101)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "InvRatTest_1" test1,
                                     TestLabel "InvRatTest_2" test2,
                                     TestLabel "InvRatTest_3" test3,
                                     TestLabel "InvRatTest_4" test4,
                                     TestLabel "InvRatTest_5" test5,
                                     TestLabel "InvRatTest_6" test6,
                                     TestLabel "RatioTest_1" test7,
                                     TestLabel "RatioTest_2" test8,
                                     TestLabel "RatioTest_3" test9,
                                     TestLabel "RatioTest_4" test10,
                                     TestLabel "RatioTest_5" test11,
                                     TestLabel "RatioTest_6" test12,
                                     TestLabel "ratioToRev_ZeroDenom_1" test13,
                                     TestLabel "ratioToRev_ZeroDenom_1" test14,
                                     TestLabel "strToRev_1" test15,
                                     TestLabel "strToRev_2" test16,
                                     TestLabel "strToRev_3" test17,
                                     TestLabel "strToRev_4" test18,
                                     TestLabel "InvShowTest_1" test19,
                                     TestLabel "InvShowTest_2" test20,
                                     TestLabel "InvShowTest_3" test21,
                                     TestLabel "InvShowTest_4" test22,
                                     TestLabel "InvShowTest_5" test23,
                                     TestLabel "InvShowTest_6" test24,
                                     TestLabel "AddTest_1" test25,
                                     TestLabel "AddTest_2" test26,
                                     TestLabel "AddTest_3" test27,
                                     TestLabel "AddTest_4" test28,
                                     TestLabel "AddTest_5" test29,
                                     TestLabel "AddTest_6" test30,
                                     TestLabel "LeftIdTest_1" test31,
                                     TestLabel "RightIdTest_1" test32,
                                     TestLabel "LeftIdTest_2" test33,
                                     TestLabel "RightIdTest_2" test34,
                                     TestLabel "LeftIdTest_3" test35,
                                     TestLabel "RightIdTest_3" test36,
                                     TestLabel "LeftInvTest_1" test37,
                                     TestLabel "RightInvTest_1" test38,
                                     TestLabel "LeftInvTest_2" test39,
                                     TestLabel "RightInvTest_2" test40,
                                     TestLabel "LeftInvTest_3" test41,
                                     TestLabel "RightInvTest_3" test42,
                                     TestLabel "scale_1" test43,
                                     TestLabel "scale_2" test44,
                                     TestLabel "scale_3" test45,
                                     TestLabel "scale_4" test46,
                                     TestLabel "scale_5" test47,
                                     TestLabel "scale_6" test48]

main = defaultMain tests
