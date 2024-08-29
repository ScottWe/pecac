module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Pecac.Parser.Revolution

-----------------------------------------------------------------------------------------
-- Valid Parsing Tests

test1 = TestCase (assertEqual "Can parse positive ratios (1/3)."
                              (5, 7)
                              (fromJust $ parseRevolution "5 / 7"))

test2 = TestCase (assertEqual "Can parse positive ratios (2/3)."
                              (123, 2)
                              (fromJust $ parseRevolution "  123   % 2"))

test3 = TestCase (assertEqual "Can parse positive ratios (3/3)."
                              (0, 3)
                              (fromJust $ parseRevolution "0%3"))

test4 = TestCase (assertEqual "Can parse negative ratios (1/3)."
                              (-6, 11)
                              (fromJust $ parseRevolution " -6/11   "))

test5 = TestCase (assertEqual "Can parse negative ratios (2/3)."
                              (-77, 2)
                              (fromJust $ parseRevolution " - 77 /2"))

test6 = TestCase (assertEqual "Can parse negative ratios (3/3)."
                              (0, 3)
                              (fromJust $ parseRevolution "-0 / 3"))

test7 = TestCase (assertEqual "Can parse ratios with explicit pi (1/2)."
                              (1, 4)
                              (fromJust $ parseRevolution "1 / 2 * pi"))

test8 = TestCase (assertEqual "Can parse ratios with explicit pi (2/2)."
                              (-99, 22)
                              (fromJust $ parseRevolution "  -  99 % 11*      pi"))

test9 = TestCase (assertEqual "Can parse integral values (1/3)."
                              (0, 1)
                              (fromJust $ parseRevolution "-0"))

test10 = TestCase (assertEqual "Can parse integral values (1/3)."
                               (1, 1)
                               (fromJust $ parseRevolution "1"))

test11 = TestCase (assertEqual "Can parse integral values (1/3)."
                               (-5, 2)
                               (fromJust $ parseRevolution "-5 * pi"))

-----------------------------------------------------------------------------------------
-- Invalid Parsing Tests

test12 = TestCase (assertEqual "Rejects misplaced negatives."
                               Nothing
                               (parseRevolution "1 / -5"))

test13 = TestCase (assertEqual "Rejects double negatives."
                               Nothing
                               (parseRevolution "-1 / -5"))

test14 = TestCase (assertEqual "Rejects expressions with more than pi."
                               Nothing
                               (parseRevolution "1 / 3 * 2 * pi"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Valid_Pos_Ratio_1" test1,
                                     TestLabel "Valid_Pos_Ratio_2" test2,
                                     TestLabel "Valid_Pos_Ratio_3" test3,
                                     TestLabel "Valid_Neg_Ratio_1" test4,
                                     TestLabel "Valid_Neg_Ratio_2" test5,
                                     TestLabel "Valid_Neg_Ratio_3" test6,
                                     TestLabel "Valid_PiExpr_1" test7,
                                     TestLabel "Valid_PiExpr_2" test8,
                                     TestLabel "Valid_IntRatio_1" test9,
                                     TestLabel "Valid_IntRatio_2" test10,
                                     TestLabel "Valid_IntRatio_3" test11,
                                     TestLabel "Invalid_IncorrectNeg" test12,
                                     TestLabel "Invalid_DoubleNeg" test13,
                                     TestLabel "Invalid_MoreThanPi" test14]

main = defaultMain tests

