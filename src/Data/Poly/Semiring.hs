-- |
-- Module:      Data.Poly.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Semiring'-based interface.
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Poly.Semiring
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , leading
  -- * Semiring interface
  , toPoly
  , monomial
  , scale
  , pattern X
  , eval
  , deriv
#if MIN_VERSION_semirings(0,5,0)
  , integral
#endif
#if MIN_VERSION_semirings(0,4,2)
  -- * Polynomials over 'Field'
  , PolyOverField(..)
  , gcdExt
  , PolyOverFractional
  , pattern PolyOverFractional
  , unPolyOverFractional
#endif
  ) where

import Data.Semiring (Semiring)
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Dense (Poly(..), VPoly, UPoly, leading)
import qualified Data.Poly.Internal.Dense as Dense
#if MIN_VERSION_semirings(0,4,2)
import Data.Poly.Internal.Dense.Field (gcdExt)
import Data.Poly.Internal.Dense.GcdDomain ()
import Data.Poly.Internal.PolyOverField
#endif
#if MIN_VERSION_semirings(0,5,0)
import Data.Euclidean (Field)
#endif

-- | Make 'Poly' from a vector of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [0,0,0] :: UPoly Int
-- 0
toPoly :: (Eq a, Semiring a, G.Vector v a) => v a -> Poly v a
toPoly = Dense.toPoly'

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a
monomial = Dense.monomial'

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 0 * X^3 + 3 * X^2 + 0 * X + 0
scale :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
scale = Dense.scale'

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
pattern X = Dense.X'

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval = Dense.eval'

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 0 * X + 3
deriv :: (Eq a, Semiring a, G.Vector v a) => Poly v a -> Poly v a
deriv = Dense.deriv'

#if MIN_VERSION_semirings(0,5,0)
-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
integral :: (Eq a, Field a, G.Vector v a) => Poly v a -> Poly v a
integral = Dense.integral'
#endif
