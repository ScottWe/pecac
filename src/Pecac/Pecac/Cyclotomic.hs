-- | Utilities for working with cyclotomic numbers.

{-# LANGUAGE TemplateHaskell #-}

module Pecac.Cyclotomic
  ( einv
  , cycOrder
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.Ratio ((%))
import Unsafe.Coerce (unsafeCoerce)
import Pecac.Maybe (maybeApply)

import qualified Data.Complex.Cyclotomic as Cyclotomic
import qualified Data.Map as Map

-----------------------------------------------------------------------------------------
-- * Cyclotomic Order.

-- | This is copied from Data.Complex.Cyclotomic
data CycInternal = CycInternal { order  :: Integer
                               , coeffs :: Map.Map Integer Rational
                               } deriving (Eq)

-- | Returns the order of a cyclotomic number, with respect to the Zumbroich basis.
--
-- Note: This function exposes the internals of Cyclotomic through unsafe Haskell. For
-- this reason, it is recommended to minimize its use.
cycOrder :: Cyclotomic.Cyclotomic -> Integer
cycOrder cyc = order internals
    where internals = unsafeCoerce cyc :: CycInternal

-----------------------------------------------------------------------------------------
-- * Natural Rational Log.

-- | Associates a target value and the smallest primitive root of unity for which it must
-- be a power if it were also a root of unity.
type SearchCtx = (Cyclotomic.Cyclotomic, Cyclotomic.Cyclotomic)

-- | Takes as input a search context and the current position in the search (cur). Using
-- the primtiive root in the context (prim), this function checks iteratively if the
-- target value can be written as prim^k * cur. If this is possible, then k is returned.
-- Otherwise, nothing is returned. The search terminates at the smallest k such that
-- (prim^k * cur == 1).
einvSearch :: SearchCtx -> Cyclotomic.Cyclotomic -> Maybe Integer
einvSearch ctx@(tar, prim) cur
    | cur == 1   = Nothing
    | tar == cur = Just 1
    | otherwise  = maybeApply (einvSearch ctx $ cur * prim) (1 +)

-- | If z is a root of unity,then returns the smallest possible n such that z is a power
-- of the primitive n-th root of unity. If the cyclotomic order of z is even, then this
-- is simply the cyclotomic order of z. Otherwise, this is twice the cyclotomic order of
-- z, since for each odd m, the (2*m)-th roots of unity can be written as rational linear
-- sums of the m-th roots of unity.
einvOrder :: Cyclotomic.Cyclotomic -> Integer
einvOrder z = if even ord then ord else ord * 2
    where ord = cycOrder z

-- | This is a partial inverse to the function Cyclotomic.e, which is defined for the
-- group of units in the ring of integers of the universal cyclotomic field. That is, for
-- each cyclotomic number z, if z is a root of unity, then returns (p/q) such that
-- z = e^(i*(p/q)*2*pi). Otherwise, nothing is returned.
einv :: Cyclotomic.Cyclotomic -> Maybe Rational
einv 1 = Just 0
einv z = maybeApply (einvSearch (z, prim) prim) ((%) ord)
    where ord  = einvOrder z
          prim = Cyclotomic.e ord
