-- | Utilities for working with affine linear functions.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

module Pecac.Affine
  ( Affine
  , Group (..)
  , Monoid (..)
  , RMod (..)
  , Semigroup (..)
  , affine
  , cfold
  , cmap
  , eval
  , getOffset
  , isConstant
  , linear
  , lit
  , skew
  , var
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Group
import Data.List (foldl')
import Pecac.List
  ( mergeWith
  , prettyList
  , repeatn
  )
import Pecac.Maybe (maybeApply)

-----------------------------------------------------------------------------------------
-- * R-Modules.

-- | A (left) R-module is an action by a ring R on an abelian group G. This action should
-- satisfy several axioms which are not checked at compile-time:
-- 1. scale s (a <> b)    == (scale s a) <> (scale s b)
-- 2. scale (r + s) a     == (scale r a) <> (scale s a)
-- 3. scale r (scale s a) == scale (r * s) a
-- 4. scale r (mempty)    == mempty
class (Num a, Abelian b) => RMod a b where
    scale :: a -> b -> b

-----------------------------------------------------------------------------------------
-- * Affine-Linear Combinations (Generalized Polynomials).

-- | Represents an affine linear sum over an R-module. That is, given a ring R and an
-- abelian group G, this type describes a function of the form c0*x0 + ... + cn*xn + a,
-- where each cj is a coefficient in R, each xj is a variable of type G, (*) is the scale
-- action of R on G, and a is a constant in G.
data (RMod a b) => Affine a b = Affine [a] b

deriving instance (RMod a b, Eq a, Eq b) => Eq (Affine a b) 

-----------------------------------------------------------------------------------------
-- * Constructors.

-- | Helper constant to refer to the additive identity.
_zero :: (Num a) => a
_zero = fromInteger 0

-- | Helper method to eliminate trailing zeros for a vector of numbers.
_reduce :: (Num a, Eq a) => [a] -> [a]
_reduce [] = []
_reduce (x:xs)
    | not $ null reduced = x : reduced
    | x == _zero         = []
    | otherwise          = [x]
    where reduced = _reduce xs

-- | The affine linear sum 0*x0 + 0*x1 + ... + 0*x(n-1) + 1*xn.
var :: (RMod a b) => Int -> Affine a b
var n = Affine coeffs mempty
    where coeffs = repeatn _zero n ++ [1]

-- | A constant affine linear sum.
lit :: (RMod a b) => b -> Affine a b
lit = Affine []

-- | Helper method to construct an affine linear function.
affine :: (RMod a b, Eq a) => [a] -> b -> Affine a b
affine coeffs offset = Affine (_reduce coeffs) offset

-- | Helper method to construct a linear function from a list of coefficients.
linear :: (RMod a b, Eq a) => [a] -> Affine a b
linear coeffs = affine coeffs mempty

-----------------------------------------------------------------------------------------
-- * Display.

instance (RMod a b, Show a, Show b) => Show (Affine a b) where
    show (Affine coeffs offset) = "{Affine " ++ cstr ++ " (" ++ ostr  ++ ")}"
        where cstr = prettyList show coeffs
              ostr = show offset

-----------------------------------------------------------------------------------------
-- * Module Structure.

-- | The affine linear functions over a countable number number of variables form a
-- semigroup under component-wise addition.
instance (RMod a b, Eq a) => Semigroup (Affine a b) where
    (<>) (Affine c1 o1) (Affine c2 o2) = affine coeffs offset
        where coeffs = mergeWith (+) _zero c1 c2
              offset = o1 <> o2

-- | The identity element for the semigroup of affine linear functions is the constant
-- function (lit mempty).
instance (RMod a b, Eq a) => Monoid (Affine a b) where
    mempty = lit mempty

-- | Every element in the monoid of affine linear functions has an inverse by taking the
-- inverse of each coefficient, and the inverse of its base-point ("offset").
instance (RMod a b, Eq a) => Group (Affine a b) where
    invert (Affine c1 o1) = affine coeffs offset
        where coeffs = map negate c1
              offset = invert o1

-- | The addition of affine linear functions is commutative, because addition in both
-- a and b are commutative.
instance (RMod a b, Eq a) => Abelian (Affine a b)

-- | Affine linear functions form an R-module, since the elements of R act on R and also
-- on the elements of G. Consequently, the elements of R act on affine linear sums via
-- component-wise multiplication.
instance (RMod a b, Eq a) => RMod a (Affine a b) where
    scale s (Affine c1 o1) = affine coeffs offset
        where coeffs = map (s *) c1
              offset = scale s o1

-- | Allows for scaling each component of the affine linear sum independently. In other
-- words, this is a skew linear transformation of the affine linear function.
skew :: (RMod a b, Eq a) => [a] -> Affine a b -> Affine a b
skew seq (Affine c1 o1) = affine c2 o1
    where c2 = mergeWith (*) _zero seq c1

-----------------------------------------------------------------------------------------
-- * Evaluation.

-- | Takes as input an affine linear function whose last non-zero coefficient is cn, and
-- a list of values of type b. If the list of values contains at least n elements, then
-- the affine linear function is evaluated on these variables (coefficients larger than
-- cn are treated as zeros, since this corresponds to the group structure on these affine
-- linear functions). Otherwise, if the list of values contains less than n elements,
-- then nothing is returned.
eval :: (RMod a b) => Affine a b -> [b] -> Maybe b
eval (Affine coeffs offset) values =
    if length coeffs <= length values
    then Just $ foldl' (<>) offset $ zipWith scale coeffs values 
    else Nothing

-- | Applies fold to the coefficients of an affine linear sum. Note that trailing zero
-- coefficients may be omitted.
cfold :: (RMod a b) => (a -> c -> c) -> c -> Affine a b -> c
cfold f v (Affine coeffs _) = foldr f v coeffs

-- | Applies map to the coefficients of an affine linear sum. Note that trailinng zero
-- coefficients may be omitted.
cmap :: (RMod a b) => (a -> c) -> Affine a b -> [c]
cmap f (Affine coeffs _) = map f coeffs

-- | Returns the offset of the affine combination.
getOffset :: (RMod a b) => Affine a b -> b
getOffset (Affine _ offset) = offset

-- | Returns true if the affine linear transformation is constant.
isConstant :: (RMod a b) => Affine a b -> Bool
isConstant (Affine coeffs _) = null coeffs
