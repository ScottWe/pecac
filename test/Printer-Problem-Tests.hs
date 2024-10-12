module Main where

import Data.Ratio
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution
import Pecac.Parser.Parser
import Pecac.Parser.Problem
import Pecac.Parser.Syntax
import Pecac.Printer.Problem

-----------------------------------------------------------------------------------------
-- Affine Linear Combinations.

aff1 :: Affine Rational Revolution
aff1 = var 0

aff2 :: Affine Rational Revolution
aff2 = var 2

expr1 :: String -> Expr
expr1 pvar = CellId pvar 0

expr2 :: String -> Expr
expr2 pvar = CellId pvar 2

-----------------------------------------------------------------------------------------
-- Gates.

-- Parameters.

pvar1 :: ParamArr
pvar1 = ParamArr "pvar" 100

pvar2 :: ParamArr
pvar2 = ParamArr "ps" 150

pdecl1 :: Stmt
pdecl1 = ParamDeclStmt $ ParamArrDecl "pvar" 100

pdecl2 :: Stmt
pdecl2 = ParamDeclStmt $ ParamArrDecl "ps" 150

-- Qubits.

qreg1 :: QubitReg
qreg1 = QubitReg "qvar" 100

qreg2 :: QubitReg
qreg2 = QubitReg "qs" 150

qdecl1 :: Stmt
qdecl1 = QubitDeclStmt $ QubitArrDecl "qvar" 100

qdecl2 :: Stmt
qdecl2 = QubitDeclStmt $ QubitArrDecl "qs" 150

-----------------------------------------------------------------------------------------
-- Gates.

-- Plain.

plain1 :: GateSummary
plain1 = PlainSummary GateX $ GateConfigs False [] [1]

plain2 :: GateSummary
plain2 = PlainSummary GateH $ GateConfigs False [] [3]

stmt_plain1 :: String -> Stmt
stmt_plain1 var = GateStmt $ Gate $ PlainGate "x" [QReg var 1]

stmt_plain2 :: String -> Stmt
stmt_plain2 var = GateStmt $ Gate $ PlainGate "h" [QReg var 3]

-- Rotation.

rot1 :: GateSummary
rot1 = RotSummary GPhase aff1 $ GateConfigs False [] []

rot2 :: GateSummary
rot2 = RotSummary RotY aff2 $ GateConfigs False [] [5]

stmt_rot1 :: String -> Stmt
stmt_rot1 pvar = GateStmt $ Gate $ RotGate "gphase" (expr1 pvar) []

stmt_rot2 :: String -> String -> Stmt
stmt_rot2 pvar qvar = GateStmt $ Gate $ RotGate "ry" (expr2 pvar) [QReg qvar 5]

-----------------------------------------------------------------------------------------
-- QASM Files.

file1 :: String -> [String] -> QASMFile
file1 ver incls = QASMFile ver incls [pdecl1, qdecl1]

file2 :: String -> [String] -> QASMFile
file2 ver incls = QASMFile ver incls [pdecl1, qdecl2, stmt_plain1 "qs"]

file3 :: String -> [String] -> QASMFile
file3 ver incls = QASMFile ver incls [pdecl2, qdecl1, stmt_rot1 "ps"]

file4 :: String -> [String] -> QASMFile
file4 ver incls = QASMFile ver incls [pdecl2, qdecl2, gstmt1, gstmt2, gstmt3, gstmt4]
    where gstmt1 = stmt_plain1 "qs"
          gstmt2 = stmt_rot1 "ps"
          gstmt3 = stmt_plain2 "qs"
          gstmt4 = stmt_rot2 "ps" "qs"

-----------------------------------------------------------------------------------------
-- encodeProblem

circ1 :: ParamCirc
circ1 = ParamCirc pvar1 qreg1 []

circ2 :: ParamCirc
circ2 = ParamCirc pvar1 qreg2 [plain1]

circ3 :: ParamCirc
circ3 = ParamCirc pvar2 qreg1 [rot1]

circ4 :: ParamCirc
circ4 = ParamCirc pvar2 qreg2 [plain1, rot1, plain2, rot2]

test1 = TestCase (assertEqual "encodeProblem works without gates"
                              (file1 "3.0" ["stdgates.inc"])
                              (encodeProblem circ1))

test2 = TestCase (assertEqual "encodeProblem works with a single plain gates"
                              (file2 "3.0" ["stdgates.inc"])
                              (encodeProblem circ2))

test3 = TestCase (assertEqual "encodeProblem works with a single rotation gates"
                              (file3 "3.0" ["stdgates.inc"])
                              (encodeProblem circ3))

test4 = TestCase (assertEqual "encodeProblem works with a single rotation gates"
                              (file4 "3.0" ["stdgates.inc"])
                              (encodeProblem circ4))

-----------------------------------------------------------------------------------------
-- Circuit Semantic Test.

testFiles :: [QASMFile]
testFiles = [file1 "3.0" ["stdgates.inc"],
             file2 "3.0" ["stdgates.inc"],
             file3 "3.0" ["stdgates.inc"],
             file4 "3.0" ["stdgates.inc"]]

circReps :: [ParamCirc]
circReps = [circ1, circ2, circ3, circ4]

circSemTestImpl n []        []          = pure ()
circSemTestImpl n (c:circs) (expt:reps) = do
    case qasmToParamCirc c of
        Left err  -> assertFailure $ errmsg err
        Right act -> assertEqual semmsg expt act
    circSemTestImpl (n + 1) circs reps
    where errmsg err = "Parse failure for circuit " ++ show n ++ ": " ++ show err
          semmsg     = "Incorrect semantic value for circuit " ++ show n ++ "."

circSemTest circs reps = circSemTestImpl 0 circs reps

test5 = TestCase $ circSemTest testFiles circReps

-----------------------------------------------------------------------------------------
-- printFile

body1 :: String
body1 = "OPENQASM 3.0;\n\n" ++
        "input array[angle, 100] pvar;\n" ++
        "qubit[100] qvar;"

body2 :: String
body2 = "OPENQASM 3;\n" ++
        "include \"stdgates.inc\";\n" ++
        "include \"mylib.inc\";\n\n" ++
        "input array[angle, 100] pvar;\n" ++
        "qubit[150] qs;\n" ++
        "x qs[1];"

body3 :: String
body3 = "OPENQASM 2.0;\n" ++
        "include \"stdgates.inc\";\n\n" ++
        "input array[angle, 150] ps;\n" ++
        "qubit[100] qvar;\n" ++
        "gphase(ps[0]);"

body4 :: String
body4 = "OPENQASM 2.0;\n" ++
        "include \"stdgates.inc\";\n" ++
        "include \"somelib.inc\";\n\n" ++
        "input array[angle, 150] ps;\n" ++
        "qubit[150] qs;\n" ++
        "x qs[1];\n" ++
        "gphase(ps[0]);\n" ++
        "h qs[3];\n" ++
        "ry(ps[2]) qs[5];"

test6 = TestCase (assertEqual "Printing of an OpenQASM file without gates."
                              body1
                              (printFile $ file1 "3.0" []))

test7 = TestCase (assertEqual "Printing of an OpenQASM file with a plain gate."
                              body2
                              (printFile $ file2 "3" ["stdgates.inc", "mylib.inc"]))

test8 = TestCase (assertEqual "Printing of an OpenQASM file with a rotation gate."
                              body3
                              (printFile $ file3 "2.0" ["stdgates.inc"]))

test9 = TestCase (assertEqual "Printing of an OpenQASM file with mixed gates."
                              body4
                              (printFile $ file4 "2.0" ["stdgates.inc", "somelib.inc"]))

-----------------------------------------------------------------------------------------
-- File Semantic Test.

testInput :: [String]
testInput = [body1, body2, body3, body4]

fileReps :: [QASMFile]
fileReps = [file1 "3.0" [],
            file2 "3" ["stdgates.inc", "mylib.inc"],
            file3 "2.0" ["stdgates.inc"],
            file4 "2.0" ["stdgates.inc", "somelib.inc"]]

fileSemTestImpl n []        []          = pure ()
fileSemTestImpl n (f:files) (expt:reps) = do
    case parseQasm "" f of
        Left err  -> assertFailure $ errmsg err
        Right act -> assertEqual semmsg expt act
    fileSemTestImpl (n + 1) files reps
    where errmsg err = "Parse failure for circuit " ++ show n ++ ": " ++ err
          semmsg     = "Incorrect semantic value for circuit " ++ show n ++ "."

fileSemTest circs reps = fileSemTestImpl 0 circs reps

test10 = TestCase $ fileSemTest testInput fileReps

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "encodeProblem_NoGates" test1,
                                     TestLabel "encodeProblem_OnePlainGate" test2,
                                     TestLabel "encodeProblem_OneRotGate" test3,
                                     TestLabel "encodeProblem_MixedGates" test4,
                                     TestLabel "SemanticValidation_encodeProblem" test5,
                                     TestLabel "printFile_1" test6,
                                     TestLabel "printFile_2" test7,
                                     TestLabel "printFile_3" test8,
                                     TestLabel "printFile_4" test9,
                                     TestLabel "SemanticValidation_printFile" test10]

main = defaultMain tests
