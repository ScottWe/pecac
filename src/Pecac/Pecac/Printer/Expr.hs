-- | Functions to print an expression tree.

module Pecac.Printer.Expr (printExpr) where

-------------------------------------------------------------------------------
-- * Import Section.

import Pecac.Parser.Syntax (Expr(..))
import Pecac.Printer.General (printCell)

-----------------------------------------------------------------------------------------
-- * Expression Printing.

-- | Helper function to print a binary expression with a connective symbol.
printBinOp :: String -> Expr -> Expr -> String
printBinOp symb lhs rhs = printExpr lhs ++ " " ++ symb ++ " " ++ printExpr rhs

-- | Converts an OpenQASM expression tree to a prettified string representation.
printExpr :: Expr -> String
printExpr (Plus lhs rhs)  = printBinOp "+" lhs rhs
printExpr (Minus lhs rhs) = printBinOp "-" lhs rhs
printExpr (Times lhs rhs) = printBinOp "*" lhs rhs
printExpr (Div lhs rhs)   = printBinOp "/" lhs rhs
printExpr (Brack expr)    = "(" ++ printExpr expr ++ ")"
printExpr (Negate expr)   = "-" ++ printExpr expr
printExpr (VarId id)      = id
printExpr (CellId id idx) = printCell id idx
printExpr (ConstNat n)    = show n
printExpr Pi              = "pi"
printExpr Tau             = "tau"
