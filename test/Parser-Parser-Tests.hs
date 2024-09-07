module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Either
import Pecac.Parser.Parser
import Pecac.Parser.Syntax

-----------------------------------------------------------------------------------------
-- Valid Parsing Tests

file :: String
file = "code.qasm"

complexExpr :: Expr
complexExpr = Plus (Plus (CellId "theta" 1)
                         (Times (ConstNat 5) (VarId "x")))
                   (Times (Brack $ Negate $ ConstNat 2) (CellId "rho" 5))

runPasmQasm :: String -> Either String QASMFile
runPasmQasm = parseQasm file

test1 = TestCase (assertEqual "Can parse version line (1/2)."
                              (Right $ QASMFile "2.0" [] [])
                              (runPasmQasm "OPENQASM 2.0;"))

test2 = TestCase (assertEqual "Can parse version line (2/2)."
                              (Right $ QASMFile "3.0" [] [])
                              (runPasmQasm "OPENQASM 3.0;"))

test3 = TestCase (assertEqual "Can parse include lines (1/3)."
                              (Right $ QASMFile "3" ["stdgates.inc"] [])
                              (runPasmQasm "include \"stdgates.inc\";"))

test4 = TestCase (assertEqual "Can parse include lines (2/3)."
                              (Right $ QASMFile "3" ["test.inc"] [])
                              (runPasmQasm "include \"test.inc\";"))

test5 = TestCase (assertEqual "Can parse include lines (3/3)."
                              (Right $ QASMFile "3" ["test.inc", "stdgates.inc"] [])
                              (runPasmQasm lines))
    where lines = "include \"test.inc\";" ++ "\n" ++
                  "include \"stdgates.inc\";"

test6 = TestCase (assertEqual "Can parse param declaration list (var)."
                              (Right $ QASMFile "3" [] [decl])
                              (runPasmQasm "input angle theta;"))
    where decl = ParamDeclStmt $ ParamVarDecl "theta"

test7 = TestCase (assertEqual "Can parse param declaration list (array)."
                              (Right $ QASMFile "3" [] [decl])
                              (runPasmQasm "input array[angle, 12] theta;"))
    where decl = ParamDeclStmt $ ParamArrDecl "theta" 12

test8 = TestCase (assertEqual "Can parse qubit declaration list (var)."
                              (Right $ QASMFile "3" [] [decl])
                              (runPasmQasm "qubit q;"))
    where decl = QubitDeclStmt $ QubitVarDecl "q"

test9 = TestCase (assertEqual "Can parse qubit declaration list (array)."
                              (Right $ QASMFile "3" [] [decl])
                              (runPasmQasm "qubit[7] qs;"))
    where decl = QubitDeclStmt $ QubitArrDecl "qs" 7

test10 = TestCase (assertEqual "Can parse qreg declaration list (var)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "qreg q1;"))
    where decl = QubitDeclStmt $ QubitVarDecl "q1"

test11 = TestCase (assertEqual "Can parse qreg declaration list (array)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "qreg qs1[77];"))
    where decl = QubitDeclStmt $ QubitArrDecl "qs1" 77

test12 = TestCase (assertEqual "Can parse basic gates (one operand)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "x qs[1];"))
    where decl = GateStmt $ Gate $ PlainGate "x" [QReg "qs" 1]

test13 = TestCase (assertEqual "Can parse basic gates (three operands)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "ccx q, qs[1], qs1[4];"))
    where decl = GateStmt $ Gate $ PlainGate "ccx" [QVar "q", QReg "qs" 1, QReg "qs1" 4]

test14 = TestCase (assertEqual "Can parse rotation gates (one operand)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "rz(theta[1]) qs[1];"))
    where expr = CellId "theta" 1
          decl = GateStmt $ Gate $ RotGate "rz" expr [QReg "qs" 1]

test15 = TestCase (assertEqual "Can parse rotation gates (three operands)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "ccrz(theta[1]) qs1[5], x, y;"))
    where expr = CellId "theta" 1
          decl = GateStmt $ Gate $ RotGate "ccrz" expr [QReg "qs1" 5, QVar "x", QVar "y"]

test16 = TestCase (assertEqual "Can parse rotation gates (complicated expression)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm line))
    where line = "rz(theta[1] + 5 * x + (-2) * rho[5]) q;"
          decl = GateStmt $ Gate $ RotGate "rz" complexExpr [QVar "q"]

test17 = TestCase (assertEqual "Can parse controlled gate."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "ctrl @ x q;"))
    where decl = GateStmt $ CtrlMod $ Gate $ PlainGate "x" [QVar "q"]

test18 = TestCase (assertEqual "Can parse negatively controlled gate."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "negctrl @ x q;"))
    where decl = GateStmt $ NegCtrlMod $ Gate $ PlainGate "x" [QVar "q"]

test19 = TestCase (assertEqual "Can parse inverse gate."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "inv @ x q;"))
    where decl = GateStmt $ InvMod $ Gate $ PlainGate "x" [QVar "q"]

test20 = TestCase (assertEqual "Can parse mixed statement (1/2)."
                               (Right $ QASMFile "2.0" ["stdgates.inc"] body)
                               (runPasmQasm lines))
    where lines = "OPENQASM 2.0;" ++ "\n" ++
                  "include \"stdgates.inc\";" ++ "\n" ++
                  "input array[  angle, 5 ] theta;" ++ "\n" ++
                  "qubit q1;" ++ "\n" ++
                  "x q1;" ++ "\n" ++
                  "qreg qs[7];" ++ "\n" ++
                  "crz(theta[1]) qs[1], q1;"
          body = [ParamDeclStmt $ ParamArrDecl "theta" 5,
                  QubitDeclStmt $ QubitVarDecl "q1",
                  GateStmt $ Gate $ PlainGate "x" [QVar "q1"],
                  QubitDeclStmt $ QubitArrDecl "qs" 7,
                  GateStmt $ Gate $ RotGate "crz" (CellId "theta" 1) [QReg "qs" 1, QVar "q1"]]

test21 = TestCase (assertEqual "Can parse mixed statement (2/2)."
                               (Right $ QASMFile "3" [] body)
                               (runPasmQasm lines))
    where lines = "input array[angle, 5] phi;" ++ "\n" ++
                  "qubit q1;" ++ "\n" ++
                  "qubit q2;" ++ "\n" ++
                  "ctrl @ x q1, q2;" ++ "\n" ++
                  "qreg qs1[22];" ++ "\n" ++
                  "crz(phi[2]) qs1[5], q2;"
          body = [ParamDeclStmt $ ParamArrDecl "phi" 5,
                  QubitDeclStmt $ QubitVarDecl "q1",
                  QubitDeclStmt $ QubitVarDecl "q2",
                  GateStmt $ CtrlMod $ Gate $ PlainGate "x" [QVar "q1", QVar "q2"],
                  QubitDeclStmt $ QubitArrDecl "qs1" 22,
                  GateStmt $ Gate $ RotGate "crz" (CellId "phi" 2) [QReg "qs1" 5, QVar "q2"]]

-----------------------------------------------------------------------------------------
-- Invalid Parsing Tests

checkValidity :: String -> Bool
checkValidity = isLeft . parseQasm file

test22 = TestCase (assertBool "Special constants are rejected as identifiers (1/6)"
                              (checkValidity "qubit pi;"))

test23 = TestCase (assertBool "Special constants are rejected as identifiers (2/6)"
                              (checkValidity "qubit π;"))

test24 = TestCase (assertBool "Special constants are rejected as identifiers (3/6)"
                              (checkValidity "qubit τ;"))

test25 = TestCase (assertBool "Special constants are rejected as identifiers (4/6)"
                              (checkValidity "qubit pi;"))

test26 = TestCase (assertBool "Special constants are rejected as identifiers (5/6)"
                              (checkValidity "qubit euler;"))

test27 = TestCase (assertBool "Special constants are rejected as identifiers (6/6)"
                              (checkValidity "qubit ℇ;"))

-----------------------------------------------------------------------------------------
-- Constant Angle Parsing

test28 = TestCase (assertEqual "Can parse rotation gates with literal pi (1/2)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "rz(pi) qs[1];"))
    where decl = GateStmt $ Gate $ RotGate "rz" Pi [QReg "qs" 1]

test29 = TestCase (assertEqual "Can parse rotation gates with literal pi (2/2)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "rz(π) qs[1];"))
    where decl = GateStmt $ Gate $ RotGate "rz" Pi [QReg "qs" 1]

test30 = TestCase (assertEqual "Can parse rotation gates with literal tau (1/2)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "rz(tau) qs[1];"))
    where decl = GateStmt $ Gate $ RotGate "rz" Tau [QReg "qs" 1]

test31 = TestCase (assertEqual "Can parse rotation gates with literal tau (2/2)."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "rz(τ) qs[1];"))
    where decl = GateStmt $ Gate $ RotGate "rz" Tau [QReg "qs" 1]

-----------------------------------------------------------------------------------------
-- Parsing Division Expressions

test32 = TestCase (assertEqual "Can parse simple division statements."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm "rz(pi / 2) q;"))
    where decl = GateStmt $ Gate $ RotGate "rz" (Div Pi $ ConstNat 2) [QVar "q"]

test33 = TestCase (assertEqual "Can parse complex division statements."
                               (Right $ QASMFile "3" [] [decl])
                               (runPasmQasm line))
    where line = "rz((theta[1] + 5 * x + (-2) * rho[5]) / (2 + 3)) q;"
          expr = Div (Brack complexExpr) (Brack $ Plus (ConstNat 2) (ConstNat 3))
          decl = GateStmt $ Gate $ RotGate "rz" expr [QVar "q"]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Valid_Version_1" test1,
                                     TestLabel "Valid_Version_2" test2,
                                     TestLabel "Valid_Include_1" test3,
                                     TestLabel "Valid_Include_2" test4,
                                     TestLabel "Valid_Include_3" test5,
                                     TestLabel "Valid_ParamVarDecl" test6,
                                     TestLabel "Valid_ParamArrDecl" test7,
                                     TestLabel "Valid_QubitVarDecl" test8,
                                     TestLabel "Valid_QubitArrDecl" test9,
                                     TestLabel "Valid_QubitVarDecl_qreg" test10,
                                     TestLabel "Valid_QubitArrDecl_qreg" test11,
                                     TestLabel "Valid_PlainGate_1" test12,
                                     TestLabel "Valid_PlainGate_2" test13,
                                     TestLabel "Valid_RotGate_1" test14,
                                     TestLabel "Valid_RotGate_2" test15,
                                     TestLabel "Valid_CompmlexExpr" test16,
                                     TestLabel "Valid_CtrlMod" test17,
                                     TestLabel "Valid_NegCtrlMod" test18,
                                     TestLabel "Valid_InvMod" test19,
                                     TestLabel "Valid_Mixed_1" test20,
                                     TestLabel "Valid_Mixed_2" test21,
                                     TestLabel "Invalid_SpecialID_1" test22,
                                     TestLabel "Invalid_SpecialID_2" test23,
                                     TestLabel "Invalid_SpecialID_3" test24,
                                     TestLabel "Invalid_SpecialID_4" test25,
                                     TestLabel "Invalid_SpecialID_5" test26,
                                     TestLabel "Invalid_SpecialID_6" test27,
                                     TestLabel "Pi_1" test28,
                                     TestLabel "Pi_2" test29,
                                     TestLabel "Tau_1" test30,
                                     TestLabel "Tau_2" test31,
                                     TestLabel "Valid_Div_1" test32,
                                     TestLabel "Valid_Div_2" test33]

main = defaultMain tests
