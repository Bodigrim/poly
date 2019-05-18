-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Num'-based interface.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

module Data.Poly
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  -- * Num interface
  , toPoly
  , constant
  , pattern X
  , eval
  , deriv
  , integral
  -- * Fractional coefficients
  , PolyOverFractional(..)
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm, (^))
import Data.Euclidean
import Data.Semiring
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G

import Data.Poly.Uni.Dense
import Data.Poly.Uni.Dense.Fractional (fractionalGcd)
import Data.Poly.Uni.Dense.GcdDomain ()

newtype PolyOverFractional poly = PolyOverFractional { unPolyOverFractional :: poly }
  deriving (Eq, Ord, Show, Num, Semiring, Semiring.Ring)

instance (Eq a, Eq (v a), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v a) => GcdDomain (PolyOverFractional (Poly v a)) where
  gcd (PolyOverFractional x) (PolyOverFractional y) = PolyOverFractional (fractionalGcd x y)
  {-# INLINE gcd #-}

instance (Eq a, Eq (v a), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v a) => Euclidean (PolyOverFractional (Poly v a)) where
  degree (PolyOverFractional x) =
    degree x
  quotRem (PolyOverFractional x) (PolyOverFractional y) =
    let (q, r) = quotRem x y in
      (PolyOverFractional q, PolyOverFractional r)
  {-# INLINE quotRem #-}
  rem (PolyOverFractional x) (PolyOverFractional y) =
    PolyOverFractional (rem x y)
  {-# INLINE rem #-}
