-- | Functiosn to convert gate summaries to syntactic objects.

module Pecac.Printer.GateSummary
  ( coeffsToExpr
  , summaryToBaseGate
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio
  ( denominator
  , numerator
  )
import Pecac.Affine
  ( Affine
  , cmap
  , getOffset
  )
import Pecac.Analyzer.Gate
  ( GateConfigs (..)
  , GateSummary (..)
  , Polarity (..)
  , plainNameToString
  , rotNameToString
  )
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , QubitReg (..)
  )
import Pecac.Analyzer.Revolution
  ( Revolution
  , asRational
  )
import Pecac.Parser.Syntax
  ( BaseGate (..)
  , Expr (..)
  , Operand (..)
  )

-----------------------------------------------------------------------------------------
-- * Expression Conversion.

-- | Constructs the integral portion of a parameter reference with coeffients. The name
-- of the parameter array, the index into the array, and the integer coefficient are
-- taken as inputs.
makeIntegralExpr :: String -> Int -> Integer -> Expr
makeIntegralExpr pvar j num =
    if num == 1
    then var
    else Times (ConstNat num) var
    where var = CellId pvar j 

-- | Constructs a parameter reference with coeffients. The name of the parameter array,
-- the index into the array, and the rational coefficient are taken as inputs.
makeRationalExpr :: String -> Int -> Rational -> Expr
makeRationalExpr pvar j q =
    if denom == 1
    then expr
    else Div expr $ ConstNat denom
    where denom = denominator q
          expr  = makeIntegralExpr pvar j $ numerator q

-- | Implementation details for expandCoeffs.
expandCoeffsImpl :: String -> Int -> [Rational] -> Expr
expandCoeffsImpl pvar j [c]    = makeRationalExpr pvar j c
expandCoeffsImpl pvar j (c:cs) = if c == 0 then rhs else Plus lhs rhs
    where lhs = makeRationalExpr pvar j c
          rhs = expandCoeffsImpl pvar (j + 1) cs

-- | Converts a list of rational coefficients to a linear sum of parameter variable
-- references. The name of the parameter array and the list of coefficients are taken as
-- inputs. If the sum is constant, then nothing is returned.
expandCoeffs :: String -> [Rational] -> Maybe Expr
expandCoeffs _    []     = Nothing
expandCoeffs pvar coeffs = Just $ expandCoeffsImpl pvar 0 coeffs

-- | Extracts the integral portion of the constant term from an affine sum.
extractIntegralOffset :: Integer -> Expr
extractIntegralOffset 1   = Tau
extractIntegralOffset num = Times (ConstNat num) Tau

-- | Extracts the constant term from an affine sum. If the offset is zero, then nothing
-- is returned.
extractOffset :: Affine Rational Revolution -> Maybe Expr
extractOffset aff
    | value == 0 = Nothing
    | denom == 1 = Just $ nexpr
    | otherwise  = Just $ Div nexpr $ ConstNat denom
    where value = asRational $ getOffset aff
          denom = denominator value
          nexpr = extractIntegralOffset $ numerator value

-- | Takes as input the name of the parameter array, and an affine sums. Returns the
-- simplest expression which would evaluate to the given affine sum.
coeffsToExpr :: String -> Affine Rational Revolution -> Expr
coeffsToExpr pvar aff =
    case maybeCoeffs of
        Just coeffs -> case maybeOffset of
            Just offset -> Plus coeffs offset
            Nothing     -> coeffs
        Nothing -> case maybeOffset of
            Just offset -> offset
            Nothing     -> ConstNat 0
    where maybeCoeffs = expandCoeffs pvar $ cmap id aff
          maybeOffset = extractOffset aff

-----------------------------------------------------------------------------------------
-- * Base Gate Conversion.

-- | Implementation details for confsToOps, which expects the operand field of the gate
-- configurations, rather than the entire gate configuration record.
confsToOpsImpl :: String -> [Int] -> [Operand]
confsToOpsImpl _    []     = []
confsToOpsImpl qvar (q:qs) = QReg qvar q : confsToOpsImpl qvar qs

-- | Converts a gate configuration into a list of operands. A string is taken as input,
-- to designate the qubit array.
confsToOps :: String -> GateConfigs -> [Operand]
confsToOps qvar (GateConfigs _ _ ops) = confsToOpsImpl qvar ops

-- | Converts a gate summary to the corresponding base gate. The parameter array and the
-- qubit register are taken as inputs, to designate the correct variables to use.
summaryToBaseGate :: ParamArr -> QubitReg -> GateSummary -> BaseGate
summaryToBaseGate _ (QubitReg qvar _) (PlainSummary name conf) = gate
    where nstr = plainNameToString name
          ops  = confsToOps qvar conf
          gate = PlainGate nstr ops
summaryToBaseGate (ParamArr pvar _) (QubitReg qvar _) (RotSummary name aff conf) = gate
    where nstr = rotNameToString name
          expr = coeffsToExpr pvar aff
          ops  = confsToOps qvar conf
          gate = RotGate nstr expr ops
