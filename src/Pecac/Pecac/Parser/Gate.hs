-- | Conversion from AST gates to their abstract representation.

module Pecac.Parser.Gate
  ( ExprErr (..)
  , GateErr (..)
  , OperandErr (..)
  , summarizeGate
  , toCoeffs
  , toPlainName
  , toQubitList
  , toRotName
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio ((%))
import Pecac.Affine
  ( Affine
  , (<>)
  , (~~)
  , invert
  , lit
  , scale
  , var
  )
import Pecac.Either
  ( branchRight
  , updateRight
  )
import Pecac.Parser.Syntax
  ( BaseGate (..)
  , Expr (..)
  , Operand (..)
  , Gate (..)
  )
import Pecac.Analyzer.Gate
  ( GateConfigs (..)
  , GateSummary (..)
  , PlainName (..)
  , Polarity (..)
  , RotName (..)
  , getPlainArity
  , getRotArity
  )
import Pecac.Analyzer.Problem
  ( ParamArr (..)
  , QubitReg (..)
  )
import Pecac.Analyzer.Revolution
  ( Revolution
  , rationalToRev
  )

-----------------------------------------------------------------------------------------
-- * Expression Abstraction.
 
-- | Explanations for expression parsing failures.
data ExprErr = IntVarUse String
             | IntArrUse String Int
             | UnknownParam String
             | ParamOOB Int Int
             | UnexpectedNat Integer
             | UnknownTimesLHS Expr
             | AngleAsInt String
             deriving (Show, Eq)

-- | Helper function to fold over a binary term in an expression tree. Takes as input the
-- left-hand side (lexpr), the right-hand side (rexpr), a function to recurse over the
-- subtrees (reader), and a function to handle the return-values, assuming that both
-- subtrees are valid (f). If (reader lexpr) and (reader lexpr) do not return
-- error-values (i.e., left values), then (f x y) is returned, where x is the right-value
-- of (reader lexpr) and y is the right-value of (reader lexpr). Otherwise, the error is
-- propogated.
binOp :: Expr -> Expr -> (Expr -> Either a b) -> (b -> b -> b) -> Either a b
binOp lexpr rexpr reader f =
    branchRight (reader lexpr) $ \lhs ->
        updateRight (reader rexpr) $ \rhs -> f lhs rhs

-- | Helper function to fold over a unary term in an expression tree. Takes as input the
-- target expression (expr), a function to recurse over this subtree (rader), and a
-- function to handle the return-value, assuming that this subtree is valid (f). If
-- (reader expr) does not return an error-value (i.e., a left value), then f is applied
-- to the right, and this new term is returned. Otherwise, the error is propogated.
unaryOp :: Expr -> (Expr -> Either a b) -> (b -> b) -> Either a b
unaryOp expr reader f = updateRight (reader expr) f

-- | Helper function to interpret sub-expressions as integer literals.
toRat :: Expr -> Either ExprErr Rational
toRat (Plus lexpr rexpr)  = binOp lexpr rexpr toRat (+)
toRat (Minus lexpr rexpr) = binOp lexpr rexpr toRat (-)
toRat (Times lexpr rexpr) = binOp lexpr rexpr toRat (*)
toRat (Div lexpr rexpr)   = binOp lexpr rexpr toRat (/)
toRat (Brack expr)        = toRat expr
toRat (Negate expr)       = unaryOp expr toRat negate
toRat (VarId name)        = Left $ IntVarUse name
toRat (CellId name idx)   = Left $ IntArrUse name idx
toRat (ConstNat n)        = Right $ (toInteger n) % 1
toRat Pi                  = Left $ AngleAsInt "pi"
toRat Tau                 = Left $ AngleAsInt "tau"

-- | Takes a summary of the parameter array together with an array access at a given
-- index. If the accessed array is the parameter array, and the index is in bounds, then
-- returns an alpha-vector corresponding to this access. Otherwise, returns an error
-- discribing why the access failed.
checkCell :: ParamArr -> String -> Int -> Either ExprErr (Affine Rational Revolution)
checkCell (ParamArr tar sz) name idx
    | tar /= name = Left $ UnknownParam name
    | idx >= sz   = Left $ ParamOOB idx sz
    | 0 > idx     = Left $ ParamOOB idx sz
    | otherwise   = Right $ var idx

-- | Possible results when analyzing the LHS of a Times expression.
data TimesLHS = Scalar Rational | Vector (Affine Rational Revolution) | TimesFailure

-- | Helper function to identify whether the left-hand side of a Times expression is an
-- integer literal expression, or a reference to one (or more) entries of the parameter
-- array. If the left-hand side is neither of these types, then nothing is returned to
-- indicate a failure.
handleTimesLhs :: ParamArr -> Expr -> TimesLHS
handleTimesLhs pvar expr =
    case toRat expr of
        Right n -> Scalar n
        _       -> case toCoeffs pvar expr of
            Right vec -> Vector vec
            _         -> TimesFailure

-- | Handles the coefficient expansion for times operators. This is a special case, since
-- either the left-hand side is a natural number while the right-hand side is an angle
-- reference, or the left-hand side is an angle reference while the right-hand side is a
-- natural number. If the left-hand side is neither of these, then a typing error has
-- occured, and a special error is raised.
timesToCoeffs :: ParamArr -> Expr -> Expr -> Either ExprErr (Affine Rational Revolution)
timesToCoeffs pvar lexpr rexpr =
    case handleTimesLhs pvar lexpr of
        Scalar n     -> updateRight (toCoeffs pvar rexpr) $ scale n
        Vector vec   -> updateRight (toRat rexpr) $ \n -> scale n vec
        TimesFailure -> Left $ UnknownTimesLHS $ Times lexpr rexpr

-- | Handles the coefficient expansion for division operators. This is a special case,
-- since the left-hand side must be an angle reference, whereas the right-hand side must
-- be a rational number. If either requirement fails, then a typing error has occured,
-- and a special error is raised.
divToCoeffs :: ParamArr -> Expr -> Expr -> Either ExprErr (Affine Rational Revolution)
divToCoeffs pvar lexpr rexpr =
    case toCoeffs pvar lexpr of
        Right vec -> updateRight (toRat rexpr) $ \r -> scale (1 / r) vec
        _         -> Left $ UnknownTimesLHS $ Div lexpr rexpr

-- | Takes as input a summary of the parameter arrays (pvar) and the parameter to a
-- rotation gate. If the expression can be interpreted as an integer linear sum of
-- entries in <tar>, then the coefficients of this linear sum are returned as an
-- alpha-vector. For example, <tar>[j] will correspond to the j-th component of the
-- alpha-vector. If such an interpretation is not possible (e.g., if the expression is
-- non-linear), then the relevant error is returned.
toCoeffs :: ParamArr -> Expr -> Either ExprErr (Affine Rational Revolution)
toCoeffs pvar (Plus lexpr rexpr)  = binOp lexpr rexpr (toCoeffs pvar) (<>)
toCoeffs pvar (Minus lexpr rexpr) = binOp lexpr rexpr (toCoeffs pvar) (~~)
toCoeffs pvar (Times lexpr rexpr) = timesToCoeffs pvar lexpr rexpr
toCoeffs pvar (Div lexpr rexpr)   = divToCoeffs pvar lexpr rexpr
toCoeffs pvar (Brack expr)        = toCoeffs pvar expr
toCoeffs pvar (Negate expr)       = unaryOp expr (toCoeffs pvar) invert
toCoeffs _    (VarId name)        = Left $ UnknownParam name
toCoeffs pvar (CellId name idx)   = checkCell pvar name idx
toCoeffs _    (ConstNat n)        = Left $ UnexpectedNat n
toCoeffs _    Pi                  = Right $ lit $ rationalToRev $ 1 % 2
toCoeffs _    Tau                 = Right $ lit $ rationalToRev 1

-----------------------------------------------------------------------------------------
-- * Operand Abstraction.

-- | Explanations for operand parsing failures.
data OperandErr = TooManyOperands Int
                | TooFewOperands Int
                | NonArrOperand
                | QubitOOB Int Int
                | UnknownQubitReg String
                | NoCloningViolation Int
                deriving (Show, Eq)

-- | Helper function to add an index to a partial list of valid operand indices. If the
-- new index already appears in the list, then no-cloning is violated, and an error is
-- returned. Otherwise, the new operand list is returned.
addOperand :: Int -> [Int] -> Either OperandErr [Int]
addOperand idx indices =
    if idx `elem` indices
    then Left $ NoCloningViolation idx
    else Right $ idx : indices

-- | Takes as input a summary of the qubit register (var), the numer of operands expected
-- by the given operator (n), and the actual list of operands. If every operand is a
-- reference to a valid registry index, with the length of the list of operands equalling
-- the number of expected operands, then a list of the corresponding indices is returned.
-- Otherwise, an error is returned detailing why such a list could not be constructed.
toQubitList :: QubitReg -> Int -> [Operand] -> Either OperandErr [Int]
toQubitList _                     0 []            = Right []
toQubitList _                     0 rst           = Left $ TooManyOperands $ length rst
toQubitList _                     n []            = Left $ TooFewOperands n
toQubitList (QubitReg     _   sz) n (QVar name:_) = Left NonArrOperand
toQubitList var@(QubitReg reg sz) n (QReg name idx:rst)
    | reg /= name = Left $ UnknownQubitReg name
    | idx >= sz   = Left $ QubitOOB idx sz
    | 0 > idx     = Left $ QubitOOB idx sz
    | otherwise   = branchRight (toQubitList var (n - 1) rst) (addOperand idx)

-----------------------------------------------------------------------------------------
-- * Gate Name Abstraction.

-- | Converts string representations of plain gate names to enumerative representations.
-- If a name is unknown, then nothing is returned.
toPlainName :: String -> Maybe PlainName
toPlainName "x"     = Just GateX
toPlainName "y"     = Just GateY
toPlainName "z"     = Just GateZ
toPlainName "h"     = Just GateH
toPlainName "s"     = Just GateS
toPlainName "sdg"   = Just GateSdg
toPlainName "t"     = Just GateT
toPlainName "tdg"   = Just GateTdg
toPlainName "sx"    = Just GateSX
toPlainName "cx"    = Just GateCX
toPlainName "cy"    = Just GateCY
toPlainName "cz"    = Just GateCZ
toPlainName "ch"    = Just GateCH
toPlainName "swap"  = Just GateSwap
toPlainName "ccx"   = Just GateCCX
toPlainName "cswap" = Just GateCSwap
toPlainName _       = Nothing

-- | Converts string representations of rotation gate names to enumerative
-- representations. If a name is unknown, then nothing is returned.
toRotName :: String -> Maybe RotName
toRotName "rx"     = Just RotX
toRotName "ry"     = Just RotY
toRotName "rz"     = Just RotZ
toRotName "crx"    = Just RotCX
toRotName "cry"    = Just RotCY
toRotName "crz"    = Just RotCZ
toRotName "gphase" = Just GPhase
toRotName _        = Nothing

-----------------------------------------------------------------------------------------
-- * Gate Abstraction.

-- | Explanations for gate parsing failures.
data GateErr = GateOperandErr OperandErr
             | GateAngleErr ExprErr
             | UnknownPlainName String
             | UnknownRotName String
             deriving (Show, Eq)

-- | Internal data structure to represent an unwrapped basic gate.
data UnwrappedGate = UnwrappedGate BaseGate Bool [Polarity]

-- | Representation for a basic gate without any modifications.
defaultGate :: BaseGate -> UnwrappedGate
defaultGate base = UnwrappedGate base False []

-- | Returns the representation for the gate obtaiend by applying an inv modifier.
invertGate :: UnwrappedGate -> UnwrappedGate
invertGate (UnwrappedGate base inv ctrls) = UnwrappedGate base (not inv) ctrls

-- | Returns the representation for the gate obtaiend by applying an ctrl modifier.
addPosCtrl :: UnwrappedGate -> UnwrappedGate
addPosCtrl (UnwrappedGate base inv ctrls) = UnwrappedGate base inv $ Pos : ctrls

-- | Returns the representation for the gate obtaiend by applying a negctrl modifier.
addNegCtrl :: UnwrappedGate -> UnwrappedGate
addNegCtrl (UnwrappedGate base inv ctrls) = UnwrappedGate base inv $ Neg : ctrls

-- | Converts a gate into its flat representation as an UnwrappedGate.
unwrapGate :: Gate -> UnwrappedGate
unwrapGate (Gate base)       = defaultGate base
unwrapGate (CtrlMod gate)    = addPosCtrl $ unwrapGate gate
unwrapGate (NegCtrlMod gate) = addNegCtrl $ unwrapGate gate
unwrapGate (InvMod gate)     = invertGate $ unwrapGate gate

-- | Internal represenation of a GateConfig object before parsing.
data RawConfigs = RawConfigs Bool [Polarity] [Operand]

-- | Attempts to convert a RawConfigs object to a GateConfigs object, given the name of
-- the qubit register and the base airty of the corresponding gate. If parsing fails,
-- then the corresponding GateErr is returned.
getConfigs :: QubitReg -> Int -> RawConfigs -> Either GateErr GateConfigs
getConfigs qvar baseAirty (RawConfigs inv ctrls ops) =
    case (toQubitList qvar arity ops) of
        Left err     -> Left $ GateOperandErr err
        Right qubits -> Right $ GateConfigs inv ctrls qubits
    where arity = baseAirty + length ctrls

-- | Implements summarizedGate for an UnwrappedGate.
summarizeGateImpl :: QubitReg -> ParamArr -> UnwrappedGate -> Either GateErr GateSummary
summarizeGateImpl qvar _ (UnwrappedGate (PlainGate name ops) inv ctrls) =
    case toPlainName name of
        Nothing -> Left $ UnknownPlainName name
        Just ty -> let raw = RawConfigs inv ctrls ops
                   in updateRight (getConfigs qvar (getPlainArity ty) raw)
                                  (PlainSummary ty)
summarizeGateImpl qvar pvar (UnwrappedGate (RotGate name expr ops) inv ctrls) =
    case toRotName name of
        Nothing -> Left $ UnknownRotName name
        Just ty -> let raw = RawConfigs inv ctrls ops
                   in branchRight (getConfigs qvar (getRotArity ty) raw) $
                        \conf -> case toCoeffs pvar expr of
                            Left err     -> Left $ GateAngleErr err
                            Right aff -> Right $ RotSummary ty aff conf

-- | Takes as input a description of the qubit registry, a description of the parameter
-- array, and the syntactic representation of a gate. If the gate is valid with respect
-- to the qubit registry and the parameter array, then a GateSummary is returned.
-- Otherwise, a GateErr is returned explaining why the gate is not valid.
summarizeGate :: QubitReg -> ParamArr -> Gate -> Either GateErr GateSummary
summarizeGate qvar pvar gate = summarizeGateImpl qvar pvar $ unwrapGate gate
