-- | General-purpose list manipulation functions not found in Prelude.

module Pecac.List
  ( getCombinations
  , mergeWith
  , prettyList
  , prettyIntercalate
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
repeatn x n = replicate n x

-- | Takes as input a binary operator, a default value, and two lists of the same type.
-- First, the smaller list is padded (at  the end) with the default value, until the two
-- lists are of the same length. Then, the two lists are zipped together using the binary
-- operation (see zipWith).
mergeWith :: (a -> a -> a) -> a -> [a] -> [a] -> [a]
mergeWith _ v []     []     = []
mergeWith f v xs     []     = map (`f` v) xs
mergeWith f v []     ys     = map (f v) ys
mergeWith f v (x:xs) (y:ys) = f x y : mergeWith f v xs ys

-- | Takes as input a list of lists (say x) of type a. Returns a list of all possible
-- sequences obtained by selecting one element from each list in x. That is, if x has
-- length n, then each element in (getCombinations x) will be a list of length n, where
-- the j-th element is selected from the j-th set in x.
-- 
-- Note that repeated values in a set are treated as distinct values. Equivalently, this
-- function operations on multisets rather than sets.
getCombinations :: [[a]] -> [[a]]
getCombinations []         = [[]]
getCombinations (set:sets) = concatMap f $ getCombinations sets
    where f seq = map (:seq) set

-----------------------------------------------------------------------------------------
-- * List Printing.

-- | Takes as input a deliminator string, a function to display elements of type a, and a
-- list of type a. Returns a deliminated string, obtained by displaying the elements in
-- the list.
prettyIntercalate :: String -> (a -> String) -> [a] -> String
prettyIntercalate tok f = intercalate tok . map f

-- | Specializes prettyIntercalate to comma deliminated lists. Spaces are included
-- between commas and elements to improve readability.
prettyItems :: (a -> String) -> [a] -> String
prettyItems = prettyIntercalate ", "

-- | Same as prettyItems, but with square brackets surrounding the list.
prettyList :: (a -> String) -> [a] -> String
prettyList f list = "[" ++ prettyItems f list ++ "]"

-- | Extends prettyItems to NonEmpty lists.
prettyNonEmpty :: (a -> String) -> NonEmpty.NonEmpty a -> String
prettyNonEmpty f = prettyItems f . NonEmpty.toList
