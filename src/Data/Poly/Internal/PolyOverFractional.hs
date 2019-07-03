-- |
-- Module:      Data.Poly.Internal.PolyOverFractional
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Wrapper with a more efficient 'Euclidean' instance.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Poly.Internal.PolyOverFractional
  ( PolyOverFractional(..)
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm, (^))
import Data.Euclidean
import Data.Semiring
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G

import qualified Data.Poly.Internal.Dense as Dense
import qualified Data.Poly.Internal.Dense.Fractional as Dense (fractionalGcd)

newtype PolyOverFractional poly = PolyOverFractional { unPolyOverFractional :: poly }
  deriving (Eq, Ord, Show, Num, Semiring, Semiring.Ring)

instance (Eq a, Eq (v a), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v a) => GcdDomain (PolyOverFractional (Dense.Poly v a)) where
  gcd (PolyOverFractional x) (PolyOverFractional y) = PolyOverFractional (Dense.fractionalGcd x y)
  {-# INLINE gcd #-}

instance (Eq a, Eq (v a), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v a) => Euclidean (PolyOverFractional (Dense.Poly v a)) where
  degree (PolyOverFractional x) =
    degree x
  quotRem (PolyOverFractional x) (PolyOverFractional y) =
    let (q, r) = quotRem x y in
      (PolyOverFractional q, PolyOverFractional r)
  {-# INLINE quotRem #-}
  rem (PolyOverFractional x) (PolyOverFractional y) =
    PolyOverFractional (rem x y)
  {-# INLINE rem #-}
