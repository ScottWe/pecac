-- | General-purpose list manipulation functions not found in Prelude.

module Pecac.List (repeatn) where

-----------------------------------------------------------------------------------------
-- * List Generation.
 
-- | Takes as input a value x and an integer n. Returns a list which repeats x n-times.
repeatn :: a -> Int -> [a]
repeatn x n = take n $ repeat x
