module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Ratio
import Pecac.Affine
import Pecac.Analyzer.Gate
import Pecac.Analyzer.Problem
import Pecac.Analyzer.Revolution

-----------------------------------------------------------------------------------------
-- addGlobalPhase

test1 = TestCase (assertEqual "addGlobalPhase handles empty circuits."
                              (ParamCirc pvar qvar [createGlobalPhase affv])
                              (addGlobalPhase affv circ))
    where pvar = ParamArr "theta" 3
          qvar = QubitReg "qs" 100
          circ = ParamCirc pvar qvar []
          affv = linear [-6, 1 % 7, 5]

test2 = TestCase (assertEqual "addGlobalPhase handles circuits with gates."
                              (ParamCirc pvar qvar $ createGlobalPhase affv : gseq)
                              (addGlobalPhase affv circ))
    where pvar = ParamArr "tau" 6
          qvar = QubitReg "qubits" 55
          gate = PlainSummary GateX $ GateConfigs False [] [5]
          gseq = [gate, gate]
          circ = ParamCirc pvar qvar gseq
          affv = affine [-100, 0, 1, 1] $ rationalToRev $ 2 % 3

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "addGlobalPhase_Empty" test1,
                                     TestLabel "addGlobalPhase_NonEmpty" test2]

main = defaultMain tests
