module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Parser.Gate
import Pecac.Parser.Syntax

-----------------------------------------------------------------------------------------
-- toCeoffs: Valid Cases

pvar :: String
pvar = "theta"

test1 = TestCase (assertEqual "Can parse a single angles without scalars (1/2)."
                              (Right [0, 0, 1, 0, 0])
                              (toCoeffs pvar 5 $ CellId pvar 2))

test2 = TestCase (assertEqual "Can parse a single angles without scalars (2/2)."
                              (Right [0, 1, 0, 0, 0, 0])
                              (toCoeffs pvar 6 $ Brack $ CellId pvar 1))

test3 = TestCase (assertEqual "Can parse a sum of angles without scalars (1/2)."
                              (Right [0, 1, 0, 1, 0])
                              (toCoeffs pvar 5 expr))
    where expr = Plus (CellId pvar 1) (CellId pvar 3)

test4 = TestCase (assertEqual "Can parse a sum of angles without scalars (2/2)."
                              (Right [0, 2, 0, 0, 0, 0])
                              (toCoeffs pvar 6 expr))
    where expr = Brack $ Plus (Brack $ CellId pvar 1) (Brack $ CellId pvar 1)

test5 = TestCase (assertEqual "Can parse a difference of angles without scalars (1/2)."
                              (Right [0, 1, -1, 0, 0])
                              (toCoeffs pvar 5 expr))
    where expr = Minus (CellId pvar 1) (CellId pvar 2)

test6 = TestCase (assertEqual "Can parse a difference of angles without scalars (2/2)."
                              (Right [0, 0, 0, 0, 0, 0])
                              (toCoeffs pvar 6 expr))
    where expr = Brack $ Minus (Brack $ CellId pvar 1) (Brack $ CellId pvar 1)

test7 = TestCase (assertEqual "Can parse a combination of three angles without scalars."
                              (Right [0, 1, 0, 1, 0, 0, -1, 0])
                              (toCoeffs pvar 8 expr))
    where expr = Minus (Plus (CellId pvar 1) (CellId pvar 3))
                       (CellId pvar 6)

test8 = TestCase (assertEqual "Can parse an angle with scalars (1/2)."
                              (Right [0, 5, 0, 0, 0, 0])
                              (toCoeffs pvar 6 expr))
    where expr = Times (CellId pvar 1) (ConstNat 5)

test9 = TestCase (assertEqual "Can parse an angle with scalars (2/2)."
                              (Right [0, 0, 6, 0, 0, 0])
                              (toCoeffs pvar 6 expr))
    where expr = Times (ConstNat 6) (CellId pvar 2)

test10 = TestCase (assertEqual "Can parse a combination of three angles with scalars."
                               (Right [0, 2, 0, 5, 0, 0, -7, 0])
                               (toCoeffs pvar 8 expr))
    where expr = Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                             (Times (ConstNat 2) (CellId pvar 1)))
                       (Times (CellId pvar 6) (ConstNat 7))

test11 = TestCase (assertEqual "Can negate a combination of angles."
                               (Right [0, -2, 0, -5, 0, 0, 7, 0])
                               (toCoeffs pvar 8 expr))
    where expr = Negate $ Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                                      (Times (ConstNat 2) (CellId pvar 1)))
                                (Times (CellId pvar 6) (ConstNat 7))

test12 = TestCase (assertEqual "Can handle coefficient expressions"
                               (Right [-6, 0, 0, 0, 0, 24])
                               (toCoeffs pvar 6 expr))
    where val1 = Negate $ Times (Minus (Brack $ ConstNat 5) (ConstNat 2)) (ConstNat 2)
          val2 = Times (Brack $ Times (ConstNat 2) (ConstNat 3)) (ConstNat 4)
          expr = Plus (Times val1 (CellId pvar 0)) (Times (CellId pvar 5) val2)

-----------------------------------------------------------------------------------------
-- toCeoffs: Invalid Cases

test13 = TestCase (assertEqual "Rejects unknown angle name."
                               (Left $ UnknownParam name)
                               (toCoeffs pvar 6 expr))
    where name = "unknown"
          expr = Plus (CellId name 0) (CellId pvar 5)

test14 = TestCase (assertEqual "Rejects non-array angle pvariable."
                               (Left $ UnknownParam pvar)
                               (toCoeffs pvar 6 expr))
    where badv = VarId pvar
          expr = Plus (CellId pvar 0) badv

test15 = TestCase (assertEqual "Rejects indices which are too large."
                               (Left $ ParamOOB badi maxi)
                               (toCoeffs pvar maxi expr))
    where maxi = 6
          badi = 10
          expr = Plus (CellId pvar 0) (CellId pvar badi)

test16 = TestCase (assertEqual "Rejects indices which are too small."
                               (Left $ ParamOOB badi maxi)
                               (toCoeffs pvar maxi expr))
    where maxi = 6
          badi = (-7)
          expr = Plus (CellId pvar 0) (CellId pvar badi)

test17 = TestCase (assertEqual "Rejects const terms."
                               (Left $ UnexpectedNat n)
                               (toCoeffs pvar 6 $ ConstNat n))
    where n = 7

test18 = TestCase (assertEqual "Integer variables are rejected (left-hand side)."
                               (Left $ UnknownTimesLHS expr)
                               (toCoeffs pvar 6 expr))
    where name = "intvar"
          expr = (Times (VarId name) (CellId pvar 0))

test19 = TestCase (assertEqual "Integer variables are rejected (right-hand side)."
                               (Left $ IntVarUse name)
                               (toCoeffs pvar 6 expr))
    where name = "intvar"
          expr = (Times (CellId pvar 0) (VarId name))

-----------------------------------------------------------------------------------------
-- toQubitList

qvar :: String
qvar = "reg"

test20 = TestCase (assertEqual "toQubitList handles empty operand lists."
                               (Right [])
                               (toQubitList qvar 100 0 []))

test21 = TestCase (assertEqual "toQubitList handles single operand lists (1/2)."
                               (Right [0])
                               (toQubitList qvar 100 1 [QReg qvar 0]))

test22 = TestCase (assertEqual "toQubitList handles single operand lists (2/2)."
                               (Right [99])
                               (toQubitList qvar 100 1 [QReg qvar 99]))

test23 = TestCase (assertEqual "toQubitList handles multi-operand lists (1/2)."
                               (Right [1, 5, 2])
                               (toQubitList qvar 100 3 ops))
    where ops = [QReg qvar 1, QReg qvar 5, QReg qvar 2]

test24 = TestCase (assertEqual "toQubitList handles multi-operand lists (2/2)."
                               (Right [10, 15, 22])
                               (toQubitList qvar 100 3 ops))
    where ops = [QReg qvar 10, QReg qvar 15, QReg qvar 22]

test25 = TestCase (assertEqual "toQubitList identifies when there are too many operands."
                               (Left $ TooManyOperands 2)
                               (toQubitList qvar 100 2 ops))
    where ops = [QReg qvar 0, QReg qvar 1, QReg qvar 2, QReg qvar 3]

test26 = TestCase (assertEqual "toQubitList identifies when there are too few operands."
                               (Left $ TooFewOperands 3)
                               (toQubitList qvar 100 7 ops))
    where ops = [QReg qvar 0, QReg qvar 1, QReg qvar 2, QReg qvar 3]

test27 = TestCase (assertEqual "toQubitList identifies when there is a scalar operand."
                               (Left NonArrOperand)
                               (toQubitList qvar 100 3 ops))
    where ops = [QReg qvar 0, QReg qvar 1, QVar qvar, QReg qvar 3]

test28 = TestCase (assertEqual "toQubitList identifies when an index is too small."
                               (Left $ QubitOOB idx 100)
                               (toQubitList qvar 100 3 ops))
    where idx = -2
          ops = [QReg qvar 0, QReg qvar 1, QReg qvar idx, QReg qvar 3]

test29 = TestCase (assertEqual "toQubitList identifies when an index is too large."
                               (Left $ QubitOOB idx 100)
                               (toQubitList qvar 100 3 ops))
    where idx = 101
          ops = [QReg qvar 0, QReg qvar 1, QReg qvar idx, QReg qvar 3]

test30 = TestCase (assertEqual "toQubitList identifies when a variable name is invalid."
                               (Left $ UnknownQubitReg var)
                               (toQubitList qvar 100 3 ops))
    where var = "unknown"
          ops = [QReg qvar 0, QReg qvar 1, QReg var 2, QReg qvar 3]

test31 = TestCase (assertEqual "toQubitList rejects operands which repeat qubit indices."
                               (Left $ NoCloningViolation idx)
                               (toQubitList qvar 100 4 ops))
    where idx = 2
          ops = [QReg qvar 0, QReg qvar 1, QReg qvar idx, QReg qvar idx]

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
                                     TestLabel "Invalid_toCoeff_IntVar_RHS" test19,
                                     TestLabel "Valid_toOperandList_Empty" test20,
                                     TestLabel "Valid_toOperandList_Single_1" test21,
                                     TestLabel "Valid_toOperandList_Single_2" test22,
                                     TestLabel "Valid_toOperandList_Multi_1" test23,
                                     TestLabel "Valid_toOperandList_Multi_2" test24,
                                     TestLabel "Invalid_toOperandList_TooMany" test25,
                                     TestLabel "Invalid_toOperandList_TooFew" test26,
                                     TestLabel "Invalid_toOperandList_Var" test27,
                                     TestLabel "Invalid_toOperandList_OOB_Small" test28,
                                     TestLabel "Invalid_toOperandList_OOB_Big" test29,
                                     TestLabel "Invalid_toOperandList_RegName" test30,
                                     TestLabel "Invalid_toOperandList_Cloning" test31]

main = defaultMain tests
