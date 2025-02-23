-- | Functions to print function delcarations.

module Pecac.Printer.FDecl (printFDecl) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Pecac.List
  ( prettyIntercalate
  , prettyItems
  )
import Pecac.Parser.Syntax
  ( FDecl (..)
  , Stmt
  )
import Pecac.Printer.Stmt (printStmt)

-----------------------------------------------------------------------------------------
-- * Function Formatting Utilities.

-- | Helper method to format the list of angle variables. This will add spacing, commas,
-- and brackets, provided that the list sis non-empty. Otherwise, a space is returned to
-- ensure proper spacing between other parts of the function declaration.
printParams :: [String] -> String
printParams [] = " "
printParams ps = " (" ++ prettyItems id ps ++ ") "

-- | Helper method to print a statement inside of a function declaration. This will print
-- the statement with a single level of indentation (4 spaces).
indentStmt :: Stmt -> String
indentStmt stmt = "    " ++ printStmt stmt

-- | Helper method to format the body of a function delcaration. This will add braces,
-- newlines, and indentation. A space will be included before the opening brace, to
-- improve readability of the declaration.
printBody :: [Stmt] -> String
printBody []   = " {\n}"
printBody body = " {\n" ++ str ++ "\n}"
    where str = prettyIntercalate "\n" indentStmt body

-----------------------------------------------------------------------------------------
-- * Function Declaration Printing.

-- | Converts an OpenQASM function declaration to a prettified string representation.
-- The block will be multiple lines.
printFDecl :: FDecl -> String
printFDecl (GateDecl name ps qs body) = "gate " ++ name ++ pstr ++ ostr ++ bstr 
    where pstr = printParams ps
          ostr = prettyItems id qs
          bstr = printBody body
