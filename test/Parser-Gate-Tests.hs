module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Ratio
import Pecac.Affine
import Pecac.Parser.Gate
import Pecac.Parser.Syntax
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution

-----------------------------------------------------------------------------------------
-- toCeoffs: Valid Cases

pvar :: String
pvar = "theta"

test1 = TestCase (assertEqual "Can parse a single angles without scalars (1/2)."
                              (Right $ linear [0, 0, 1, 0, 0])
                              (toCoeffs (ParamArr pvar 5) $ CellId pvar 2))

test2 = TestCase (assertEqual "Can parse a single angles without scalars (2/2)."
                              (Right $ linear [0, 1, 0, 0, 0, 0])
                              (toCoeffs (ParamArr pvar 6) $ Brack $ CellId pvar 1))

test3 = TestCase (assertEqual "Can parse a sum of angles without scalars (1/2)."
                              (Right $ linear [0, 1, 0, 1, 0])
                              (toCoeffs (ParamArr pvar 5) expr))
    where expr = Plus (CellId pvar 1) (CellId pvar 3)

test4 = TestCase (assertEqual "Can parse a sum of angles without scalars (2/2)."
                              (Right $ linear [0, 2, 0, 0, 0, 0])
                              (toCoeffs (ParamArr pvar 6) expr))
    where expr = Brack $ Plus (Brack $ CellId pvar 1) (Brack $ CellId pvar 1)

test5 = TestCase (assertEqual "Can parse a difference of angles without scalars (1/2)."
                              (Right $ linear [0, 1, -1, 0, 0])
                              (toCoeffs (ParamArr pvar 5) expr))
    where expr = Minus (CellId pvar 1) (CellId pvar 2)

test6 = TestCase (assertEqual "Can parse a difference of angles without scalars (2/2)."
                              (Right $ linear [0, 0, 0, 0, 0, 0])
                              (toCoeffs (ParamArr pvar 6) expr))
    where expr = Brack $ Minus (Brack $ CellId pvar 1) (Brack $ CellId pvar 1)

test7 = TestCase (assertEqual "Can parse a combination of three angles without scalars."
                              (Right $ linear [0, 1, 0, 1, 0, 0, -1, 0])
                              (toCoeffs (ParamArr pvar 8) expr))
    where expr = Minus (Plus (CellId pvar 1) (CellId pvar 3))
                       (CellId pvar 6)

test8 = TestCase (assertEqual "Can parse an angle with scalars (1/2)."
                              (Right $ linear [0, 5, 0, 0, 0, 0])
                              (toCoeffs (ParamArr pvar 6) expr))
    where expr = Times (CellId pvar 1) (ConstNat 5)

test9 = TestCase (assertEqual "Can parse an angle with scalars (2/2)."
                              (Right $ linear [0, 0, 6, 0, 0, 0])
                              (toCoeffs (ParamArr pvar 6) expr))
    where expr = Times (ConstNat 6) (CellId pvar 2)

test10 = TestCase (assertEqual "Can parse a combination of three angles with scalars."
                               (Right $ linear [0, 2, 0, 5, 0, 0, -7, 0])
                               (toCoeffs (ParamArr pvar 8) expr))
    where expr = Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                             (Times (ConstNat 2) (CellId pvar 1)))
                       (Times (CellId pvar 6) (ConstNat 7))

test11 = TestCase (assertEqual "Can negate a combination of angles."
                               (Right $ linear [0, -2, 0, -5, 0, 0, 7, 0])
                               (toCoeffs (ParamArr pvar 8) expr))
    where expr = Negate $ Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                                      (Times (ConstNat 2) (CellId pvar 1)))
                                (Times (CellId pvar 6) (ConstNat 7))

test12 = TestCase (assertEqual "Can handle coefficient expressions"
                               (Right $ linear [-6, 0, 0, 0, 0, 24])
                               (toCoeffs (ParamArr pvar 6) expr))
    where val1 = Negate $ Times (Minus (Brack $ ConstNat 5) (ConstNat 2)) (ConstNat 2)
          val2 = Times (Brack $ Times (ConstNat 2) (ConstNat 3)) (ConstNat 4)
          expr = Plus (Times val1 (CellId pvar 0)) (Times (CellId pvar 5) val2)

-----------------------------------------------------------------------------------------
-- toCeoffs: Invalid Cases

test13 = TestCase (assertEqual "Rejects unknown angle name."
                               (Left $ UnknownParam name)
                               (toCoeffs (ParamArr pvar 6) expr))
    where name = "unknown"
          expr = Plus (CellId name 0) (CellId pvar 5)

test14 = TestCase (assertEqual "Rejects non-array angle pvariable."
                               (Left $ UnknownParam pvar)
                               (toCoeffs (ParamArr pvar 6) expr))
    where badv = VarId pvar
          expr = Plus (CellId pvar 0) badv

test15 = TestCase (assertEqual "Rejects indices which are too large."
                               (Left $ ParamOOB badi maxi)
                               (toCoeffs (ParamArr pvar maxi) expr))
    where maxi = 6
          badi = 10
          expr = Plus (CellId pvar 0) (CellId pvar badi)

test16 = TestCase (assertEqual "Rejects indices which are too small."
                               (Left $ ParamOOB badi maxi)
                               (toCoeffs (ParamArr pvar maxi) expr))
    where maxi = 6
          badi = (-7)
          expr = Plus (CellId pvar 0) (CellId pvar badi)

test17 = TestCase (assertEqual "Rejects const terms."
                               (Left $ UnexpectedNat n)
                               (toCoeffs (ParamArr pvar 6) $ ConstNat n))
    where n = 7

test18 = TestCase (assertEqual "Integer variables are rejected (left-hand side)."
                               (Left $ UnknownTimesLHS expr)
                               (toCoeffs (ParamArr pvar 6) expr))
    where name = "intvar"
          expr = (Times (VarId name) (CellId pvar 0))

test19 = TestCase (assertEqual "Integer variables are rejected (right-hand side)."
                               (Left $ IntVarUse name)
                               (toCoeffs (ParamArr pvar 6) expr))
    where name = "intvar"
          expr = (Times (CellId pvar 0) (VarId name))

-----------------------------------------------------------------------------------------
-- toQubitList

qvar :: String
qvar = "reg"

test20 = TestCase (assertEqual "toQubitList handles empty operand lists."
                               (Right [])
                               (toQubitList (QubitReg qvar 100) 0 []))

test21 = TestCase (assertEqual "toQubitList handles single operand lists (1/2)."
                               (Right [0])
                               (toQubitList (QubitReg qvar 100) 1 [QReg qvar 0]))

test22 = TestCase (assertEqual "toQubitList handles single operand lists (2/2)."
                               (Right [99])
                               (toQubitList (QubitReg qvar 100) 1 [QReg qvar 99]))

test23 = TestCase (assertEqual "toQubitList handles multi-operand lists (1/2)."
                               (Right [1, 5, 2])
                               (toQubitList (QubitReg qvar 100) 3 ops))
    where ops = [QReg qvar 1, QReg qvar 5, QReg qvar 2]

test24 = TestCase (assertEqual "toQubitList handles multi-operand lists (2/2)."
                               (Right [10, 15, 22])
                               (toQubitList (QubitReg qvar 100) 3 ops))
    where ops = [QReg qvar 10, QReg qvar 15, QReg qvar 22]

test25 = TestCase (assertEqual "toQubitList identifies when there are too many operands."
                               (Left $ TooManyOperands 2)
                               (toQubitList (QubitReg qvar 100) 2 ops))
    where ops = [QReg qvar 0, QReg qvar 1, QReg qvar 2, QReg qvar 3]

test26 = TestCase (assertEqual "toQubitList identifies when there are too few operands."
                               (Left $ TooFewOperands 3)
                               (toQubitList (QubitReg qvar 100) 7 ops))
    where ops = [QReg qvar 0, QReg qvar 1, QReg qvar 2, QReg qvar 3]

test27 = TestCase (assertEqual "toQubitList identifies when there is a scalar operand."
                               (Left NonArrOperand)
                               (toQubitList (QubitReg qvar 100) 3 ops))
    where ops = [QReg qvar 0, QReg qvar 1, QVar qvar, QReg qvar 3]

test28 = TestCase (assertEqual "toQubitList identifies when an index is too small."
                               (Left $ QubitOOB idx 100)
                               (toQubitList (QubitReg qvar 100) 3 ops))
    where idx = -2
          ops = [QReg qvar 0, QReg qvar 1, QReg qvar idx, QReg qvar 3]

test29 = TestCase (assertEqual "toQubitList identifies when an index is too large."
                               (Left $ QubitOOB idx 100)
                               (toQubitList (QubitReg qvar 100) 3 ops))
    where idx = 101
          ops = [QReg qvar 0, QReg qvar 1, QReg qvar idx, QReg qvar 3]

test30 = TestCase (assertEqual "toQubitList identifies when a variable name is invalid."
                               (Left $ UnknownQubitReg var)
                               (toQubitList (QubitReg qvar 100) 3 ops))
    where var = "unknown"
          ops = [QReg qvar 0, QReg qvar 1, QReg var 2, QReg qvar 3]

test31 = TestCase (assertEqual "toQubitList rejects operands which repeat qubit indices."
                               (Left $ NoCloningViolation idx)
                               (toQubitList (QubitReg qvar 100) 4 ops))
    where idx = 2
          ops = [QReg qvar 0, QReg qvar 1, QReg qvar idx, QReg qvar idx]

-----------------------------------------------------------------------------------------
-- summarizeGate: Valid Cases

parr :: ParamArr
parr = ParamArr pvar 5

qreg :: QubitReg
qreg = QubitReg qvar 100

test32 = TestCase (assertEqual "Can summarize a plain gate without modifiers (1/2)."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = Gate $ PlainGate "x" [QReg qvar 10]
          summ = PlainSummary GateX $ GateConfigs False [] [10]

test33 = TestCase (assertEqual "Can summarize a plain gate without modifiers (2/2)."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = Gate $ PlainGate "ccx" [QReg qvar 10, QReg qvar 2, QReg qvar 5]
          summ = PlainSummary GateCCX $ GateConfigs False [] [10, 2, 5]

test34 = TestCase (assertEqual "Can summarize a rotation gate without modifiers (1/2)."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where expr = Times (ConstNat 5) (CellId pvar 2)
          gate = Gate $ RotGate "rx" expr [QReg qvar 10]
          aff  = linear [0, 0, 5, 0, 0]
          summ = RotSummary RotX aff $ GateConfigs False [] [10]

test35 = TestCase (assertEqual "Can summarize a rotation gate without modifiers (2/2)."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where expr = Plus (Times (ConstNat 5) (CellId pvar 2)) (Negate $ CellId pvar 0)
          gate = Gate $ RotGate "crx" expr [QReg qvar 10, QReg qvar 59]
          aff  = linear [-1, 0, 5, 0, 0]
          summ = RotSummary RotCX aff $ GateConfigs False [] [10, 59]

test36 = TestCase (assertEqual "Can summarize a plain gate with a ctrl modifier."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = CtrlMod $ Gate $ PlainGate "x" [QReg qvar 10, QReg qvar 2]
          summ = PlainSummary GateX $ GateConfigs False [Pos] [10, 2]

test37 = TestCase (assertEqual "Can summarize a plain gate with a negctrl modifier."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where qops = [QReg qvar 10, QReg qvar 2, QReg qvar 7, QReg qvar 99]
          gate = NegCtrlMod $ Gate $ PlainGate "ccx" qops
          summ = PlainSummary GateCCX $ GateConfigs False [Neg] [10, 2, 7, 99]

test38 = TestCase (assertEqual "Can summarize a plain gate with an inv modifier."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = InvMod $ Gate $ PlainGate "x" [QReg qvar 10]
          summ = PlainSummary GateX $ GateConfigs True [] [10]

test39 = TestCase (assertEqual "Can summarize a plain gate with two inv modifiers."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = InvMod $ InvMod $ Gate $ PlainGate "x" [QReg qvar 10]
          summ = PlainSummary GateX $ GateConfigs False [] [10]

test40 = TestCase (assertEqual "Can summarize a plain gate with mixed modifiers (1/2)."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where qops = [QReg qvar 10, QReg qvar 2, QReg qvar 77, QReg qvar 5]
          gate = NegCtrlMod $ InvMod $ CtrlMod $ Gate $ PlainGate "cx" qops
          summ = PlainSummary GateCX $ GateConfigs True [Neg, Pos] [10, 2, 77, 5]

test41 = TestCase (assertEqual "Can summarize a plain gate with mixed modifiers (2/2)."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where qops = [QReg qvar 10, QReg qvar 5, QReg qvar 2, QReg qvar 4]
          gate = CtrlMod $ InvMod $ NegCtrlMod $ InvMod $ Gate $ PlainGate "cx" qops
          summ = PlainSummary GateCX $ GateConfigs False [Pos, Neg] [10, 5, 2, 4]

test42 = TestCase (assertEqual "Can summarize a rotation gate with mixed modifiers."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where expr = Plus (Times (ConstNat 5) (CellId pvar 2)) (CellId pvar 3)
          qops = [QReg qvar 1, QReg qvar 2, QReg qvar 3]
          gate = NegCtrlMod $ InvMod $ CtrlMod $ Gate $ RotGate "rx" expr qops
          aff  = linear [0, 0, 5, 1, 0]
          summ = RotSummary RotX aff $ GateConfigs True [Neg, Pos] [1, 2, 3]

-----------------------------------------------------------------------------------------
-- summarizeGate: Invalid Cases

test43 = TestCase (assertEqual "Rejects summarization for unknown plain names."
                               (Left $ UnknownPlainName name)
                               (summarizeGate qreg parr gate))
    where name = "bad"
          gate = Gate $ PlainGate name [QReg qvar 10]

test44 = TestCase (assertEqual "Rejects summarization for unknown rotation names."
                               (Left $ UnknownRotName name)
                               (summarizeGate qreg parr gate))
    where name = "bad"
          expr = Times (ConstNat 5) (CellId pvar 2)
          gate = Gate $ RotGate name expr [QReg qvar 10]

test45 = TestCase (assertEqual "Rejects plain gates with too many operands (1/2)."
                               (Left $ GateOperandErr $ TooManyOperands 1)
                               (summarizeGate qreg parr gate))
    where gate = Gate $ PlainGate "x" [QReg qvar 10, QReg qvar 12]

test46 = TestCase (assertEqual "Rejects plain gates with too few operands (2/2)."
                               (Left $ GateOperandErr $ TooFewOperands 2)
                               (summarizeGate qreg parr gate))
    where gate = Gate $ PlainGate "ccx" [QReg qvar 10]

test47 = TestCase (assertEqual "Rejects plain gates with invalid angles."
                               (Left $ GateAngleErr $ ParamOOB badi maxi)
                               (summarizeGate qreg (ParamArr pvar maxi) gate))
    where maxi = 6
          badi = (-7)
          expr = Plus (CellId pvar 0) (CellId pvar badi)
          gate = Gate $ RotGate "crx" expr [QReg qvar 10, QReg qvar 59]

-----------------------------------------------------------------------------------------
-- toCeoffs: constants

pival :: Revolution
pival = rationalToRev $ 1 % 2

test48 = TestCase (assertEqual "toCeoffs handles the value pi."
                               (Right $ lit pival)
                               (toCoeffs (ParamArr pvar 8) Pi))

test49 = TestCase (assertEqual "toCeoffs handles the value tau."
                               (Right $ lit $ rationalToRev 1)
                               (toCoeffs (ParamArr pvar 8) Tau))

test50 = TestCase (assertEqual "toCeoffs handles affine linear sums."
                               (Right $ affine [0, -2, 0, -5, 0, 0, 7, 0] pival)
                               (toCoeffs (ParamArr pvar 8) $ Plus lexpr Pi))
    where lexpr = Negate $ Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                                       (Times (ConstNat 2) (CellId pvar 1)))
                                 (Times (CellId pvar 6) (ConstNat 7))

test51 = TestCase (assertEqual "toCeoffs rejects multiplication by anlges (1/2)."
                               (Left $ AngleAsInt "pi")
                               (toCoeffs (ParamArr pvar 8) $ Times lexpr Pi))
    where lexpr = Negate $ Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                                       (Times (ConstNat 2) (CellId pvar 1)))
                                 (Times (CellId pvar 6) (ConstNat 7))

test52 = TestCase (assertEqual "toCeoffs rejects multiplication by anlges (2/2)."
                               (Left $ AngleAsInt "tau")
                               (toCoeffs (ParamArr pvar 8) $ Times lexpr Tau))
    where lexpr = Negate $ Minus (Plus (Times (ConstNat 5) (CellId pvar 3))
                                       (Times (ConstNat 2) (CellId pvar 1)))
                                 (Times (CellId pvar 6) (ConstNat 7))

-----------------------------------------------------------------------------------------
-- toCeoffs: rationals

test53 = TestCase (assertEqual "toCoeffs handles rational coefficients."
                               (Right $ linear [0, 5 % 2, 0, 0, 0, 0])
                               (toCoeffs (ParamArr pvar 6) expr))
    where expr = Times (CellId pvar 1) (Div (ConstNat 5) (ConstNat 2))

test54 = TestCase (assertEqual "toCoeffs handles dividing angles by rationals."
                               (Right $ linear [0, 2 % 5, 0, 0, 0, 0])
                               (toCoeffs (ParamArr pvar 6) expr))
    where expr = Div (CellId pvar 1) (Div (ConstNat 5) (ConstNat 2))

-----------------------------------------------------------------------------------------
-- summarizeGate: gphase

test55 = TestCase (assertEqual "Can summarize a gphase gate without controls."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = Gate $ RotGate "gphase" Pi []
          aff  = lit pival
          summ = RotSummary GPhase aff $ GateConfigs False [] []

test56 = TestCase (assertEqual "Can summarize a gphase gate with a control."
                               (Right summ)
                               (summarizeGate qreg parr gate))
    where gate = CtrlMod $ Gate $ RotGate "gphase" Pi [QReg qvar 0]
          aff  = lit pival
          summ = RotSummary GPhase aff $ GateConfigs False [Pos] [0]

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
                                     TestLabel "Invalid_toOperandList_Cloning" test31,
                                     TestLabel "Valid_summarizeGate_Plain_1" test32,
                                     TestLabel "Valid_summarizeGate_Plain_2" test33,
                                     TestLabel "Valid_summarizeGate_Rot_1" test34,
                                     TestLabel "Valid_summarizeGate_Rot_2" test35,
                                     TestLabel "Valid_summarizeGate_Plain_Ctrl" test36,
                                     TestLabel "Valid_summarizeGate_Plain_NCtrl" test37,
                                     TestLabel "Valid_summarizeGate_Plain_Inv" test38,
                                     TestLabel "Valid_summarizeGate_Plain_InvInv" test39,
                                     TestLabel "Valid_summarizeGate_Plain_Mixed_1" test40,
                                     TestLabel "Valid_summarizeGate_Plain_Mixed_2" test41,
                                     TestLabel "Valid_summarizeGate_Rot_Mixed" test42,
                                     TestLabel "Invalid_summarizeGate_PlainName" test43,
                                     TestLabel "Invalid_summarizeGate_RotName" test44,
                                     TestLabel "Invalid_summarizeGate_OpCount_1" test45,
                                     TestLabel "Invalid_summarizeGate_OpCount_2" test46,
                                     TestLabel "Invalid_summarizeGate_AngleErr" test47,
                                     TestLabel "Valid_toCoeffs_Pi" test48,
                                     TestLabel "Valid_toCoeffs_Tau" test49,
                                     TestLabel "Valid_toCoeffs_Affine" test50,
                                     TestLabel "Invalid_toCoeff_AngleAsInt_1" test51,
                                     TestLabel "Invalid_toCoeff_AngleAsInt_2" test52,
                                     TestLabel "Valid_toCoeffs_Div_1" test53,
                                     TestLabel "Valid_toCoeffs_Div_2" test54,
                                     TestLabel "Valid_summarizeGate_GPhase" test55,
                                     TestLabel "Valid_summarizeGate_GPhase_Ctrl" test56]

main = defaultMain tests
