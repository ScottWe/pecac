module Main where

import Data.Ratio
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution
import Pecac.Parser.Syntax
import Pecac.Printer.GateSummary

-----------------------------------------------------------------------------------------
-- Affine Linear Combinations.

aff1 :: Affine Rational Revolution
aff1 = var 0

aff2 :: Affine Rational Revolution
aff2 = var 2

aff3 :: Affine Rational Revolution
aff3 = lit $ rationalToRev 3

aff4 :: Affine Rational Revolution
aff4 = lit $ rationalToRev $ -3

aff5 :: Affine Rational Revolution
aff5 = lit $ rationalToRev $ 3 % 4

aff6 :: Affine Rational Revolution
aff6 = var 2 <> var 4

aff7 :: Affine Rational Revolution
aff7 = skew [1%2, 2%3, 0, -5] base
    where base = var 0 <> var 1 <> var 3

aff8 :: Affine Rational Revolution
aff8 = litv <> skew [2%3, 0, 1%5, 5] base
    where base = var 0 <> var 2 <> var 3
          litv = lit $ rationalToRev $ 5 % 2

expr1 :: String -> Expr
expr1 pvar = CellId pvar 0

expr2 :: String -> Expr
expr2 pvar = CellId pvar 2

expr3 :: Expr
expr3 = Times (ConstNat 3) Tau

expr4 :: Expr
expr4 = Times (ConstNat $ -3) Tau

expr5 :: Expr
expr5 = Div (Times (ConstNat 3) Tau) (ConstNat 4)

expr6 :: String -> Expr
expr6 pvar = Plus (CellId pvar 2) (CellId pvar 4)

expr7 :: String -> Expr
expr7 pvar = Plus term1 $ Plus term2 term3
    where term1 = Div (CellId pvar 0) (ConstNat 2)
          term2 = Div (Times (ConstNat 2) (CellId pvar 1)) (ConstNat 3)
          term3 = Times (ConstNat $ -5) (CellId pvar 3)

expr8 :: String -> Expr
expr8 pvar = Plus (Plus term1 (Plus term2 term3)) const
    where term1 = Div (Times (ConstNat 2) (CellId pvar 0)) (ConstNat 3)
          term2 = Div (CellId pvar 2) (ConstNat 5)
          term3 = Times (ConstNat 5) (CellId pvar 3)
          const = Div (Times (ConstNat 5) Tau) (ConstNat 2)

-----------------------------------------------------------------------------------------
-- Gates.

pvar1 :: ParamArr
pvar1 = ParamArr "pvar" 100

pvar2 :: ParamArr
pvar2 = ParamArr "ps" 150

qreg1 :: QubitReg
qreg1 = QubitReg "qvar" 100

qreg2 :: QubitReg
qreg2 = QubitReg "qs" 150

-----------------------------------------------------------------------------------------
-- Gates.

-- Plain Gates - No Modifiers.

plain1 :: GateSummary
plain1 = PlainSummary GateX $ GateConfigs False [] [1]

plain2 :: GateSummary
plain2 = PlainSummary GateH $ GateConfigs False [] [3]

plain3 :: GateSummary
plain3 = PlainSummary GateCCX $ GateConfigs False [] [0, 5, 6]

base_plain1 :: String -> BaseGate
base_plain1 var = PlainGate "x" [QReg var 1]

base_plain2 :: String -> BaseGate
base_plain2 var = PlainGate "h" [QReg var 3]

base_plain3 :: String -> BaseGate
base_plain3 var = PlainGate "ccx" [QReg var 0, QReg var 5, QReg var 6]

-- Plain Gates - Modifiers.

plainmod1 :: GateSummary
plainmod1 = PlainSummary GateCX $ GateConfigs False [Neg] [1, 2, 3]

plainmod2 :: GateSummary
plainmod2 = PlainSummary GateSX $ GateConfigs True [Pos] [0, 1, 2]

plainmod3 :: GateSummary
plainmod3 = PlainSummary GateY $ GateConfigs True [Pos, Neg, Pos] [2, 4, 6, 8]

base_plainmod1 :: String -> BaseGate
base_plainmod1 var = PlainGate "cx" [QReg var 1, QReg var 2, QReg var 3]

base_plainmod2 :: String -> BaseGate
base_plainmod2 var = PlainGate "sx" [QReg var 0, QReg var 1, QReg var 2]

base_plainmod3 :: String -> BaseGate
base_plainmod3 var = PlainGate "y" [QReg var 2, QReg var 4, QReg var 6, QReg var 8]

-- Rotation Gates - No Modifiers.

rot1 :: GateSummary
rot1 = RotSummary GPhase aff1 $ GateConfigs False [] []

rot2 :: GateSummary
rot2 = RotSummary RotY aff2 $ GateConfigs False [] [5]

rot3 :: GateSummary
rot3 = RotSummary RotCX aff3 $ GateConfigs False [] [0, 1]

base_rot1 :: String -> BaseGate
base_rot1 pvar = RotGate "gphase" (expr1 pvar) []

base_rot2 :: String -> String -> BaseGate
base_rot2 pvar qvar = RotGate "ry" (expr2 pvar) [QReg qvar 5]

base_rot3 :: String -> String -> BaseGate
base_rot3 pvar qvar = RotGate "crx" expr3 [QReg qvar 0, QReg qvar 1]

-- Rotation Gates - Modifiers.

rotmod1 :: GateSummary
rotmod1 = RotSummary RotP aff6 $ GateConfigs False [Neg] [1, 2]

rotmod2 :: GateSummary
rotmod2 = RotSummary RotCP aff7 $ GateConfigs True [Pos] [0, 1, 2]

rotmod3 :: GateSummary
rotmod3 = RotSummary RotZ aff8 $ GateConfigs True [Pos, Neg] [2, 4, 6]

base_rotmod1 :: String -> String -> BaseGate
base_rotmod1 pvar qvar = RotGate "p" (expr6 pvar) [QReg qvar 1, QReg qvar 2]

base_rotmod2 :: String -> String -> BaseGate
base_rotmod2 pvar qvar = RotGate "cp" (expr7 pvar) [QReg qvar 0, QReg qvar 1, QReg qvar 2]

base_rotmod3 :: String -> String -> BaseGate
base_rotmod3 pvar qvar = RotGate "rz" (expr8 pvar) [QReg qvar 2, QReg qvar 4, QReg qvar 6]

-----------------------------------------------------------------------------------------
-- coeffsToExpr

test1 = TestCase (assertEqual "coeffsToExpr handles single params without coeffs (1/2)."
                              (expr1 "pvar")
                              (coeffsToExpr "pvar" aff1))

test2 = TestCase (assertEqual "coeffsToExpr handles single params without coeffs (2/2)."
                              (expr2 "params")
                              (coeffsToExpr "params" aff2))

test3 = TestCase (assertEqual "coeffsToExpr handles constant expressions (1/3)."
                              expr3
                              (coeffsToExpr "pvar" aff3))

test4 = TestCase (assertEqual "coeffsToExpr handles constant expressions (2/3)."
                              expr4
                              (coeffsToExpr "params" aff4))

test5 = TestCase (assertEqual "coeffsToExpr handles constant expressions (3/3)."
                              expr5
                              (coeffsToExpr "ps" aff5))

test6 = TestCase (assertEqual "coeffsToExpr handles sums of expressions."
                              (expr6 "ps")
                              (coeffsToExpr "ps" aff6))

test7 = TestCase (assertEqual "coeffsToExpr handles multiple parameters with coeffients."
                              (expr7 "pvar")
                              (coeffsToExpr "pvar" aff7))

test8 = TestCase (assertEqual "coeffsToExpr handles expressions with all types of terms."
                              (expr8 "ps")
                              (coeffsToExpr "ps" aff8))

-----------------------------------------------------------------------------------------
-- summaryToBaseGate: Plain Gates.

test9 = TestCase (assertEqual "summaryToBaseGate handles plain unmodified gates (1/3)."
                              (base_plain1 "qvar")
                              (summaryToBaseGate pvar1 qreg1 plain1))

test10 = TestCase (assertEqual "summaryToBaseGate handles plain unmodified gates (2/3)."
                               (base_plain2 "qs")
                               (summaryToBaseGate pvar1 qreg2 plain2))

test11 = TestCase (assertEqual "summaryToBaseGate handles plain unmodified gates (3/3)."
                               (base_plain3 "qvar")
                               (summaryToBaseGate pvar2 qreg1 plain3))

test12 = TestCase (assertEqual "summaryToBaseGate handles plain modified gates (1/3)."
                               (base_plainmod1 "qs")
                               (summaryToBaseGate pvar2 qreg2 plainmod1))

test13 = TestCase (assertEqual "summaryToBaseGate handles plain modified gates (2/3)."
                               (base_plainmod2 "qvar")
                               (summaryToBaseGate pvar1 qreg1 plainmod2))

test14 = TestCase (assertEqual "summaryToBaseGate handles plain modified gates (3/3)."
                               (base_plainmod3 "qvar")
                               (summaryToBaseGate pvar1 qreg1 plainmod3))

-----------------------------------------------------------------------------------------
-- summaryToBaseGate: Rotation Gates.

test15 = TestCase (assertEqual "summaryToBaseGate handles unmodified rot gates (1/3)."
                               (base_rot1 "pvar")
                               (summaryToBaseGate pvar1 qreg1 rot1))

test16 = TestCase (assertEqual "summaryToBaseGate handles unmodified rot gates (2/3)."
                               (base_rot2 "pvar" "qs")
                               (summaryToBaseGate pvar1 qreg2 rot2))

test17 = TestCase (assertEqual "summaryToBaseGate handles unmodified rot gates (3/3)."
                               (base_rot3 "ps" "qvar")
                               (summaryToBaseGate pvar2 qreg1 rot3))

test18 = TestCase (assertEqual "summaryToBaseGate handles modified rot gates (1/3)."
                               (base_rotmod1 "ps" "qs")
                               (summaryToBaseGate pvar2 qreg2 rotmod1))

test19 = TestCase (assertEqual "summaryToBaseGate handles modified rot gates (2/3)."
                               (base_rotmod2 "pvar" "qvar")
                               (summaryToBaseGate pvar1 qreg1 rotmod2))

test20 = TestCase (assertEqual "summaryToBaseGate handles modified rot gates (3/3)."
                               (base_rotmod3 "pvar" "qvar")
                               (summaryToBaseGate pvar1 qreg1 rotmod3))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "coeffsToExpr_OneVar_1" test1,
                                     TestLabel "coeffsToExpr_OneVar_1" test2,
                                     TestLabel "coeffsToExpr_Const_1" test3,
                                     TestLabel "coeffsToExpr_Const_2" test4,
                                     TestLabel "coeffsToExpr_Const_3" test5,
                                     TestLabel "coeffsToExpr_TwoVar" test6,
                                     TestLabel "coeffsToExpr_ThreeVar" test7,
                                     TestLabel "coeffsToExpr_Mixed" test8,
                                     TestLabel "summaryToBaseGate_Plain_1" test9,
                                     TestLabel "summaryToBaseGate_Plain_2" test10,
                                     TestLabel "summaryToBaseGate_Plain_3" test11,
                                     TestLabel "summaryToBaseGate_Plain_Mod_1" test12,
                                     TestLabel "summaryToBaseGate_Plain_Mod_2" test13,
                                     TestLabel "summaryToBaseGate_Plain_Mod_3" test14,
                                     TestLabel "summaryToBaseGate_Rot_1" test15,
                                     TestLabel "summaryToBaseGate_Rot_2" test16,
                                     TestLabel "summaryToBaseGate_Rot_3" test17,
                                     TestLabel "summaryToBaseGate_Rot_Mod_1" test18,
                                     TestLabel "summaryToBaseGate_Rot_Mod_2" test19,
                                     TestLabel "summaryToBaseGate_Rot_Mod_3" test20]

main = defaultMain tests
