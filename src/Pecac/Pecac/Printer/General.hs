-- | Functions to print general syntax elements.

module Pecac.Printer.General (printCell) where

-----------------------------------------------------------------------------------------
-- * General Printing.

-- | Function to print a cell access into an array.
printCell :: String -> Int -> String
printCell name id = name ++ "[" ++ show id ++ "]"
