-- | General-purpose list manipulation functions not found in Prelude.

module Pecac.List
  ( prettyList
  , prettyItems
  , prettyNonEmpty
  , repeatn
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.List (intercalate)

import qualified Data.List.NonEmpty as NonEmpty

-----------------------------------------------------------------------------------------
-- * List Generation.
 
-- | Takes as input a value x and an integer n. Returns a list which repeats x n-times.
repeatn :: a -> Int -> [a]
repeatn x n = take n $ repeat x

-----------------------------------------------------------------------------------------
-- * List Printing.

-- | Takes as input a function to display elements of type a, and a list of type a.
-- Returns a comma-deliminated string, obtained by displaying the elements in the list.
-- Spaces are included between commas and elements to improve readability.
prettyItems :: (a -> String) -> [a] -> String
prettyItems f = intercalate ", " . map f

-- | Same as prettyItems, but with square brackets surrounding the list.
prettyList :: (a -> String) -> [a] -> String
prettyList f list = "[" ++ prettyItems f list ++ "]"

-- | Extends prettyItems to NonEmpty lists.
prettyNonEmpty :: (a -> String) -> NonEmpty.NonEmpty a -> String
prettyNonEmpty f = prettyItems f . NonEmpty.toList
