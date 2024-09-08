-- | Functions to print a gate.

module Pecac.Printer.Gate (printGate) where

-------------------------------------------------------------------------------
-- * Import Section.

import Pecac.List (prettyItems)
import Pecac.Parser.Syntax
  ( BaseGate (..)
  , Gate (..)
  , Operand (..)
  )
import Pecac.Printer.Expr (printExpr)
import Pecac.Printer.General (printCell)

-----------------------------------------------------------------------------------------
-- * Gate Printing.

-- | Formats an operand without spaces.
printOperand :: Operand -> String
printOperand (QVar id)     = id
printOperand (QReg id idx) = printCell id idx

-- | Converts a list of operands to a comma deliminated list of their prettified string
-- representations, as dictated by (printOperand).
printOperands :: [Operand] -> String
printOperands = prettyItems printOperand

-- | Function to print a gate instance (either the name of a plain gate, or the name of a
-- rotation gate including its stringified argument), followed by its stringified
-- operands, where both the instance and operands are given as arguments.
printGateInst :: String -> [Operand] -> String
printGateInst inst []  = inst
printGateInst inst ops = inst ++ " " ++ printOperands ops

-- | Function to print a prettified string represenation of a base gate.
printBaseGate :: BaseGate -> String
printBaseGate (PlainGate name ops)    = printGateInst name ops
printBaseGate (RotGate name expr ops) = printGateInst inst ops
    where inst = name ++ "(" ++ printExpr expr ++ ")"

-- | Function to print a gate modifier (specified by a string) followed by a
-- prettified string representation of the gate.
printMod :: String -> Gate -> String
printMod mod gate = mod ++ " @ " ++ printGate gate

-- | Converts an OpenQASM gate to a prettified string representation.
printGate :: Gate -> String
printGate (Gate base)       = printBaseGate base
printGate (CtrlMod gate)    = printMod "ctrl" gate
printGate (NegCtrlMod gate) = printMod "negctrl" gate
printGate (InvMod gate)     = printMod "inv" gate
