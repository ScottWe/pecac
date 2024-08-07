-- | Conversion from AST gates to their abstract representation.

module Pecac.Parser.Gate
  ( ExprErr (..)
  , OperandErr (..)
  , toCoeffs
  , toPlainName
  , toQubitList
  , toRotName
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Either
  ( branchRight
  , updateRight
  )
import Pecac.List (repeatn)
import Pecac.Parser.Syntax
  ( Expr (..)
  , Operand (..)
  )
import Pecac.Analyzer.Gate
  ( PlainName (..)
  , RotName (..)
  )

-----------------------------------------------------------------------------------------
-- * Expression Abstraction.
 
-- | Explanations for expression parsing failures.
data ExprErr = IntVarUse String
             | IntArrUse String Int
             | UnknownParam String
             | ParamOOB Int Int
             | UnexpectedNat Int
             | UnknownTimesLHS Expr
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
toInt :: Expr -> Either ExprErr Int
toInt (Plus lexpr rexpr)  = binOp lexpr rexpr toInt (+)
toInt (Minus lexpr rexpr) = binOp lexpr rexpr toInt (-)
toInt (Times lexpr rexpr) = binOp lexpr rexpr toInt (*)
toInt (Brack expr)        = toInt expr
toInt (Negate expr)       = unaryOp expr toInt $ \x -> -x
toInt (VarId name)        = Left $ IntVarUse name
toInt (CellId name idx)   = Left $ IntArrUse name idx
toInt (ConstNat n)        = Right n

-- | Takes as input the name of the parameter array, and its length, together with an
-- array access at a given index. If the accessed array is the parameter array, and the
-- index is in bounds, then returns an alpha-vector corresponding to this access.
-- Otherwise, returns an error discribing why the access failed.
checkCell :: String -> Int -> String -> Int -> Either ExprErr [Int]
checkCell tar sz name idx
    | tar /= name = Left $ UnknownParam name
    | idx >= sz   = Left $ ParamOOB idx sz
    | 0 > idx     = Left $ ParamOOB idx sz
    | otherwise   = Right $ lhs ++ (1 : rhs)
    where lhs = repeatn 0 idx
          rhs = repeatn 0 $ sz - idx - 1

-- | Possible results when analyzing the LHS of a Times expression.
data TimesLHS = Scalar Int | Vector [Int] | TimesFailure

-- | Helper function to identify whether the left-hand side of a Times expression is an
-- integer literal expression, or a reference to one (or more) entries of the parameter
-- array. If the left-hand side is neither of these types, then nothing is returned to
-- indicate a failure.
handleTimesLhs :: String -> Int -> Expr -> TimesLHS
handleTimesLhs tar sz expr =
    case toInt expr of
        Right n -> Scalar n
        _       -> case toCoeffs tar sz expr of
            Right vec -> Vector vec
            _         -> TimesFailure

-- | Handles the coefficient expansion for times operators. This is a special case, since
-- either the left-hand side is a natural number while the right-hand side is an angle
-- reference, or the left-hand side is an angle reference while the right-hand side is a
-- natural number. If the left-hand side is neither of these, then a typing error has
-- occured, and a special error is raised.
timesToCoeffs :: String -> Int -> Expr -> Expr -> Either ExprErr [Int]
timesToCoeffs tar sz lexpr rexpr =
    case handleTimesLhs tar sz lexpr of
        Scalar n     -> updateRight (toCoeffs tar sz rexpr) $ scale n
        Vector vec   -> updateRight (toInt rexpr) $ \n -> scale n vec
        TimesFailure -> Left $ UnknownTimesLHS (Times lexpr rexpr)
    where scale n vec = map (n *) vec

-- | Takes as input the name of the parameter array (tar), the size of the parameter
-- array (sz), an the parameter to a rotation gate. If the expression can be interpreted
-- as an integer linear sum of entries in <tar>, then the coefficients of this linear sum
-- are returned as an alpha-vector. For example, <tar>[j] will correspond to the j-th
-- component of the alpha-vector. If such an interpretation is not possible (e.g., if the
-- expression is non-linear), then the relevant error is returned.
toCoeffs :: String -> Int -> Expr -> Either ExprErr [Int]
toCoeffs tar sz (Plus lexpr rexpr)  = binOp lexpr rexpr (toCoeffs tar sz) $ zipWith (+)
toCoeffs tar sz (Minus lexpr rexpr) = binOp lexpr rexpr (toCoeffs tar sz) $ zipWith (-)
toCoeffs tar sz (Times lexpr rexpr) = timesToCoeffs tar sz lexpr rexpr
toCoeffs tar sz (Brack expr)        = toCoeffs tar sz expr
toCoeffs tar sz (Negate expr)       = unaryOp expr (toCoeffs tar sz) $ map negate
toCoeffs _   _  (VarId name)        = Left $ UnknownParam name
toCoeffs tar sz (CellId name idx)   = checkCell tar sz name idx
toCoeffs _   _  (ConstNat n)        = Left $ UnexpectedNat n

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

-- | Takes as input the name of the qubit registry (reg), the size of the registry (sz),
-- the numer of operands expected by the given operator (n), and the actual list of
-- operands. If every operand is a reference to a valid registry index, with the length
-- of the list of operands equalling the number of expected operands, then a list of the
-- corresponding indices is returned. Otherwise, an error is returned detailing why such
-- a list could not be constructed.
toQubitList :: String -> Int -> Int -> [Operand] -> Either OperandErr [Int]
toQubitList _   _  0 []            = Right []
toQubitList _   _  0 rst           = Left $ TooManyOperands $ length rst
toQubitList _   _  n []            = Left $ TooFewOperands n
toQubitList _   sz n (QVar name:_) = Left NonArrOperand
toQubitList reg sz n (QReg name idx:rst)
    | reg /= name = Left $ UnknownQubitReg name
    | idx >= sz   = Left $ QubitOOB idx sz
    | 0 > idx     = Left $ QubitOOB idx sz
    | otherwise   = branchRight (toQubitList reg sz (n - 1) rst) (addOperand idx)

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
toRotName "rx"  = Just RotX
toRotName "ry"  = Just RotY
toRotName "rz"  = Just RotZ
toRotName "crx" = Just RotCX
toRotName "cry" = Just RotCY
toRotName "crz" = Just RotCZ
toRotName _     = Nothing
