module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Parser.Gate
import Pecac.Parser.Problem
import Pecac.Parser.Syntax

-----------------------------------------------------------------------------------------
-- summarizeStmts: Valid Cases

qname :: String
qname = "reg"

gate1 :: Stmt
gate1 = GateStmt $ Gate $ PlainGate "x" [QReg qname 1]

gate2 :: Stmt
gate2 = GateStmt $ Gate $ PlainGate "y" [QReg qname 2]

gate3 :: Stmt
gate3 = GateStmt $ Gate $ PlainGate "z" [QReg qname 3]

summ1 :: GateSummary
summ1 = PlainSummary GateX $ GateConfigs False [] [1]

summ2 :: GateSummary
summ2 = PlainSummary GateY $ GateConfigs False [] [2]

summ3 :: GateSummary
summ3 = PlainSummary GateZ $ GateConfigs False [] [3]

test1 = TestCase (assertEqual "Can parse a list of statements with zero gates (1/2)."
                              (Right $ ParamCirc pvar qvar [])
                              (summarizeStmts [pdecl, qdecl]))
    where pname = "theta"
          psize = 5
          qsize = 100
          pdecl = ParamDeclStmt $ ParamArrDecl pname psize
          qdecl = QubitDeclStmt $ QubitArrDecl qname qsize
          pvar  = ParamArr pname psize
          qvar  = QubitReg qname qsize

test2 = TestCase (assertEqual "Can parse a list of statements with zero gates (2/2)."
                              (Right $ ParamCirc pvar qvar [])
                              (summarizeStmts [pdecl, qdecl]))
    where pname = "angles"
          qname = "qubits"
          psize = 10
          qsize = 50
          pdecl = ParamDeclStmt $ ParamArrDecl pname psize
          qdecl = QubitDeclStmt $ QubitArrDecl qname qsize
          pvar  = ParamArr pname psize
          qvar  = QubitReg qname qsize

test3 = TestCase (assertEqual "Can parse a list of statements with a single gate."
                              (Right $ ParamCirc pvar qvar [summ1])
                              (summarizeStmts [pdecl, qdecl, gate1]))
    where pname = "theta"
          psize = 10
          qsize = 50
          pdecl = ParamDeclStmt $ ParamArrDecl pname psize
          qdecl = QubitDeclStmt $ QubitArrDecl qname qsize
          pvar  = ParamArr pname psize
          qvar  = QubitReg qname qsize

test4 = TestCase (assertEqual "Can parse a list of statements with three gates."
                              (Right $ ParamCirc pvar qvar [summ1, summ2, summ3])
                              (summarizeStmts [pdecl, qdecl, gate1, gate2, gate3]))
    where pname = "theta"
          psize = 10
          qsize = 50
          pdecl = ParamDeclStmt $ ParamArrDecl pname psize
          qdecl = QubitDeclStmt $ QubitArrDecl qname qsize
          pvar  = ParamArr pname psize
          qvar  = QubitReg qname qsize

-----------------------------------------------------------------------------------------
-- summarizeStmts: Invalid Cases

test5 = TestCase (assertEqual "summarizeStmts rejects duplicated variable names."
                              (Left $ DuplicateName pname)
                              (summarizeStmts [pdecl, qdecl]))
    where pname = "theta"
          psize = 5
          qsize = 100
          pdecl = ParamDeclStmt $ ParamArrDecl pname psize
          qdecl = QubitDeclStmt $ QubitArrDecl pname qsize

test6 = TestCase (assertEqual "summarizeStmts rejects missing param declaration (1/3)."
                              (Left MissingParams)
                              (summarizeStmts []))

test7 = TestCase (assertEqual "summarizeStmts rejects missing param declaration (2/3)."
                              (Left MissingParams)
                              (summarizeStmts [qdecl]))
    where qdecl = QubitDeclStmt $ QubitArrDecl qname 100

test8 = TestCase (assertEqual "summarizeStmts rejects missing param declaration (3/3)."
                              (Left MissingParams)
                              (summarizeStmts [qdecl, gate1]))
    where qdecl = QubitDeclStmt $ QubitArrDecl qname 100

test9 = TestCase (assertEqual "summarizeStmts rejects missing qubit declaration (1/2)."
                              (Left MissingQubits)
                              (summarizeStmts [pdecl]))
    where pdecl = ParamDeclStmt $ ParamArrDecl "theta" 5

test10 = TestCase (assertEqual "summarizeStmts rejects missing qubit declaration (2/2)."
                               (Left MissingQubits)
                               (summarizeStmts [pdecl, gate2]))
    where pdecl = ParamDeclStmt $ ParamArrDecl "theta" 5

test11 = TestCase (assertEqual "summarizeStmts rejects unexpected param declarations."
                               (Left $ UnexpectParamDecl bdecl)
                               (summarizeStmts [pdecl, qdecl, gate1, bstmt, gate2]))
    where pdecl = ParamDeclStmt $ ParamArrDecl "theta" 5
          qdecl = QubitDeclStmt $ QubitArrDecl qname 100
          bdecl = ParamArrDecl "theta2" 4
          bstmt = ParamDeclStmt bdecl

test12 = TestCase (assertEqual "summarizeStmts rejects unexpected qubit declarations."
                               (Left $ UnexpectQubitDecl bdecl)
                               (summarizeStmts [pdecl, qdecl, gate1, bstmt, gate2]))
    where pdecl = ParamDeclStmt $ ParamArrDecl "theta" 5
          qdecl = QubitDeclStmt $ QubitArrDecl qname 100
          bdecl = QubitArrDecl "reg2" 54
          bstmt = QubitDeclStmt bdecl

test13 = TestCase (assertEqual "summarizeStmts rejects non-array parameter varaibles."
                               (Left NonArrParamDecl)
                               (summarizeStmts [pdecl, qdecl, gate1]))
    where pdecl = ParamDeclStmt $ ParamVarDecl "theta"
          qdecl = QubitDeclStmt $ QubitArrDecl qname 50

test14 = TestCase (assertEqual "summarizeStmts rejects non-array qubit varaibles."
                               (Left NonArrQubitDecl)
                               (summarizeStmts [pdecl, qdecl, gate1]))
    where pdecl = ParamDeclStmt $ ParamArrDecl "theta" 5
          qdecl = QubitDeclStmt $ QubitVarDecl qname

test15 = TestCase (assertEqual "summarizeStmts rejects invalid gates."
                               (Left $ InvalidGate bgate $ UnknownPlainName bname)
                               (summarizeStmts [pdecl, qdecl, gate1, bdecl, gate3]))
    where bname = "bad"
          pdecl = ParamDeclStmt $ ParamArrDecl "theta" 5
          qdecl = QubitDeclStmt $ QubitArrDecl qname 100
          bgate = Gate $ PlainGate bname [QReg qname 10]
          bdecl = GateStmt bgate

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Valid_summarizeStmts_NoGates_1" test1,
                                     TestLabel "Valid_summarizeStmts_NoGates_2" test2,
                                     TestLabel "Valid_summarizeStmts_1Gate" test3,
                                     TestLabel "Valid_summarizeStmts_3Gate" test4,
                                     TestLabel "Invalid_summarizeStmts_DupName" test5,
                                     TestLabel "Invalid_summarizeStmts_NoPDecl_1" test6,
                                     TestLabel "Invalid_summarizeStmts_NoPDecl_2" test7,
                                     TestLabel "Invalid_summarizeStmts_NoPDecl_3" test8,
                                     TestLabel "Invalid_summarizeStmts_NoQDecl_1" test9,
                                     TestLabel "Invalid_summarizeStmts_NoQDecl_2" test10,
                                     TestLabel "Invalid_summarizeStmts_DupParam" test11,
                                     TestLabel "Invalid_summarizeStmts_DupQubit" test12,
                                     TestLabel "Invalid_summarizeStmts_Para Var" test13,
                                     TestLabel "Invalid_summarizeStmts_QubitVar" test14,
                                     TestLabel "Invalid_summarizeStmts_BadGate" test15]

main = defaultMain tests
