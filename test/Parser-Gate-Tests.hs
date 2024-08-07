module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Parser.Gate
import Pecac.Parser.Syntax

-----------------------------------------------------------------------------------------
-- toCeoffs: Valid Cases

var :: String
var = "theta"

test1 = TestCase (assertEqual "Can parse a single angles without scalars (1/2)."
                              (Right [0, 0, 1, 0, 0])
                              (toCoeffs var 5 $ CellId var 2))

test2 = TestCase (assertEqual "Can parse a single angles without scalars (2/2)."
                              (Right [0, 1, 0, 0, 0, 0])
                              (toCoeffs var 6 $ Brack $ CellId var 1))

test3 = TestCase (assertEqual "Can parse a sum of angles without scalars (1/2)."
                              (Right [0, 1, 0, 1, 0])
                              (toCoeffs var 5 expr))
    where expr = Plus (CellId var 1) (CellId var 3)

test4 = TestCase (assertEqual "Can parse a sum of angles without scalars (2/2)."
                              (Right [0, 2, 0, 0, 0, 0])
                              (toCoeffs var 6 expr))
    where expr = Brack $ Plus (Brack $ CellId var 1) (Brack $ CellId var 1)

test5 = TestCase (assertEqual "Can parse a difference of angles without scalars (1/2)."
                              (Right [0, 1, -1, 0, 0])
                              (toCoeffs var 5 expr))
    where expr = Minus (CellId var 1) (CellId var 2)

test6 = TestCase (assertEqual "Can parse a difference of angles without scalars (2/2)."
                              (Right [0, 0, 0, 0, 0, 0])
                              (toCoeffs var 6 expr))
    where expr = Brack $ Minus (Brack $ CellId var 1) (Brack $ CellId var 1)

test7 = TestCase (assertEqual "Can parse a combination of three angles without scalars."
                              (Right [0, 1, 0, 1, 0, 0, -1, 0])
                              (toCoeffs var 8 expr))
    where expr = Minus (Plus (CellId var 1) (CellId var 3))
                       (CellId var 6)

test8 = TestCase (assertEqual "Can parse an angle with scalars (1/2)."
                              (Right [0, 5, 0, 0, 0, 0])
                              (toCoeffs var 6 expr))
    where expr = Times (CellId var 1) (ConstNat 5)

test9 = TestCase (assertEqual "Can parse an angle with scalars (2/2)."
                              (Right [0, 0, 6, 0, 0, 0])
                              (toCoeffs var 6 expr))
    where expr = Times (ConstNat 6) (CellId var 2)

test10 = TestCase (assertEqual "Can parse a combination of three angles with scalars."
                               (Right [0, 2, 0, 5, 0, 0, -7, 0])
                               (toCoeffs var 8 expr))
    where expr = Minus (Plus (Times (ConstNat 5) (CellId var 3))
                             (Times (ConstNat 2) (CellId var 1)))
                       (Times (CellId var 6) (ConstNat 7))

test11 = TestCase (assertEqual "Can negate a combination of angles."
                               (Right [0, -2, 0, -5, 0, 0, 7, 0])
                               (toCoeffs var 8 expr))
    where expr = Negate $ Minus (Plus (Times (ConstNat 5) (CellId var 3))
                                      (Times (ConstNat 2) (CellId var 1)))
                                (Times (CellId var 6) (ConstNat 7))

test12 = TestCase (assertEqual "Can handle coefficient expressions"
                               (Right [-6, 0, 0, 0, 0, 24])
                               (toCoeffs var 6 expr))
    where val1 = Negate $ Times (Minus (Brack $ ConstNat 5) (ConstNat 2)) (ConstNat 2)
          val2 = Times (Brack $ Times (ConstNat 2) (ConstNat 3)) (ConstNat 4)
          expr = Plus (Times val1 (CellId var 0)) (Times (CellId var 5) val2)

-----------------------------------------------------------------------------------------
-- toCeoffs: Invalid Cases

test13 = TestCase (assertEqual "Rejects unknown angle name."
                               (Left $ UnknownParam name)
                               (toCoeffs var 6 expr))
    where name = "unknown"
          expr = Plus (CellId name 0) (CellId var 5)

test14 = TestCase (assertEqual "Rejects non-array angle variable."
                               (Left $ UnknownParam var)
                               (toCoeffs var 6 expr))
    where badv = VarId var
          expr = Plus (CellId var 0) badv

test15 = TestCase (assertEqual "Rejects indices which are too large."
                               (Left $ ParamOOB badi maxi)
                               (toCoeffs var maxi expr))
    where maxi = 6
          badi = 10
          expr = Plus (CellId var 0) (CellId var badi)

test16 = TestCase (assertEqual "Rejects indices which are too small."
                               (Left $ ParamOOB badi maxi)
                               (toCoeffs var maxi expr))
    where maxi = 6
          badi = (-7)
          expr = Plus (CellId var 0) (CellId var badi)

test17 = TestCase (assertEqual "Rejects const terms."
                               (Left $ UnexpectedNat n)
                               (toCoeffs var 6 $ ConstNat n))
    where n = 7

test18 = TestCase (assertEqual "Integer variables are rejected (left-hand side)."
                               (Left $ UnknownTimesLHS expr)
                               (toCoeffs var 6 expr))
    where name = "intvar"
          expr = (Times (VarId name) (CellId var 0))

test19 = TestCase (assertEqual "Integer variables are rejected (right-hand side)."
                               (Left $ IntVarUse name)
                               (toCoeffs var 6 expr))
    where name = "intvar"
          expr = (Times (CellId var 0) (VarId name))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Valid_toCoeff_Angle_1" test1,
                                     TestLabel "Valid_toCoeff_Angle_2" test2,
                                     TestLabel "Valid_toCoeff_Sum_1" test3,
                                     TestLabel "Valid_toCoeff_Sum_2" test4,
                                     TestLabel "Valid_toCoeff_Minus_1" test5,
                                     TestLabel "Valid_toCoeff_Minus_2" test6,
                                     TestLabel "Valid_toCoeff_3Term" test7,
                                     TestLabel "Valid_toCoeff_Coeff_1" test8,
                                     TestLabel "Valid_toCoeff_Coeff_2" test9,
                                     TestLabel "Valid_toCoeff_LinIntSeq" test10,
                                     TestLabel "Valid_toCoeff_Negate" test11,
                                     TestLabel "Valid_toCoeff_IntExpr" test12,
                                     TestLabel "Invalid_toCoeff_BadAngle" test13,
                                     TestLabel "Invalid_toCoeff_AngleVar" test14,
                                     TestLabel "Invalid_toCoeff_OOB_Big" test15,
                                     TestLabel "Invalid_toCoeff_OOB_Small" test16,
                                     TestLabel "Invalid_toCoeff_Const" test17,
                                     TestLabel "Invalid_toCoeff_IntVar_LHS" test18,
                                     TestLabel "Invalid_toCoeff_IntVar_RHS" test19]

main = defaultMain tests
