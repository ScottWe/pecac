-- | Utilities for working with cyclotomic numbers.

{-# LANGUAGE TemplateHaskell #-}

module Pecac.Cyclotomic (cycOrder) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Unsafe.Coerce (unsafeCoerce)

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
