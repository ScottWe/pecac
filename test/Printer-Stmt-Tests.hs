module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Parser.Syntax
import Pecac.Printer.Stmt

-----------------------------------------------------------------------------------------
-- printQubitDecl

test1 = TestCase (assertEqual "Can print a scalar qubit declaration (1/2)."
                              "qubit q"
                              (printQubitDecl $ QubitVarDecl "q"))

test2 = TestCase (assertEqual "Can print a scalar qubit declaration (2/2)."
                              "qubit var"
                              (printQubitDecl $ QubitVarDecl "var"))

test3 = TestCase (assertEqual "Can print an array qubit declaration (1/2)."
                              "qubit[5] reg"
                              (printQubitDecl $ QubitArrDecl "reg" 5))

test4 = TestCase (assertEqual "Can print an array qubit declaration (2/2)."
                              "qubit[7] qs"
                              (printQubitDecl $ QubitArrDecl "qs" 7))

-----------------------------------------------------------------------------------------
-- printParamDecl

test5 = TestCase (assertEqual "Can print a scalar angle declaration (1/2)."
                              "input angle theta"
                              (printParamDecl $ ParamVarDecl "theta"))

test6 = TestCase (assertEqual "Can print a scalar angle declaration (2/2)."
                              "input angle var"
                              (printParamDecl $ ParamVarDecl "var"))

test7 = TestCase (assertEqual "Can print an array angle declaration (1/2)."
                              "input array[angle, 5] thetas"
                              (printParamDecl $ ParamArrDecl "thetas" 5))

test8 = TestCase (assertEqual "Can print an array angle declaration (2/2)."
                              "input array[angle, 7] vars"
                              (printParamDecl $ ParamArrDecl "vars" 7))

-----------------------------------------------------------------------------------------
-- printStmt

test9 = TestCase (assertEqual "Can print a qubit declaration statement (1/2)."
                              "qubit q;"
                              (printStmt $ QubitDeclStmt $ QubitVarDecl "q"))

test10 = TestCase (assertEqual "Can print a qubit declaration statement (2/2)."
                               "qubit[4] qs;"
                               (printStmt $ QubitDeclStmt $ QubitArrDecl "qs" 4))

test11 = TestCase (assertEqual "Can print a param declaration statement (1/2)."
                               "input angle theta;"
                               (printStmt $ ParamDeclStmt $ ParamVarDecl "theta"))

test12 = TestCase (assertEqual "Can print a param declaration statement (2/2)."
                               "input array[angle, 7] as;"
                               (printStmt $ ParamDeclStmt $ ParamArrDecl "as" 7))

test13 = TestCase (assertEqual "Can print a gate statement (1/2)."
                               "x qs;"
                               (printStmt $ GateStmt $ Gate $ PlainGate "x" [QVar "qs"]))

test14 = TestCase (assertEqual "Can print a gate statement (2/2)."
                               "rz(2 * param[1]) qs[10];"
                               (printStmt $ GateStmt $ Gate gate))
    where expr = Times (ConstNat 2) (CellId "param" 1)
          gate = RotGate "rz" expr [QReg "qs" 10]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "printQubitDecl_Var_1" test1,
                                     TestLabel "printQubitDecl_Var_2" test2,
                                     TestLabel "printQubitDecl_Arr_1" test3,
                                     TestLabel "printQubitDecl_Arr_2" test4,
                                     TestLabel "printParamDecl_Var_1" test5,
                                     TestLabel "printParamDecl_Var_2" test6,
                                     TestLabel "printParamDecl_Arr_1" test7,
                                     TestLabel "printParamDecl_Arr_2" test8,
                                     TestLabel "printStmt_QubitDeclStmt_1" test9,
                                     TestLabel "printStmt_QubitDeclStmt_2" test10,
                                     TestLabel "printStmt_ParamDeclStmt_1" test11,
                                     TestLabel "printStmt_ParamDeclStmt_2" test12,
                                     TestLabel "printStmt_GateStmt_1" test13,
                                     TestLabel "printStmt_GateStmt_1" test14]

main = defaultMain tests
