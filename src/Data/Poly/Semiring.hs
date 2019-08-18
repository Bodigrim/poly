-- |
-- Module:      Data.Poly.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Semiring'-based interface.
--

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

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
#if MIN_VERSION_semirings(0,4,2)
  , gcdExt
  -- * Fractional coefficients
  , PolyOverFractional(..)
  , fractionalGcdExt
  , scaleMonic
#endif
  ) where

import Data.Semiring (Ring, Semiring)
import qualified Data.Vector.Generic as G
#if MIN_VERSION_semirings(0,4,2)
import Data.Euclidean (Euclidean)
#endif

import Data.Poly.Internal.Dense (Poly(..), VPoly, UPoly, leading)
import qualified Data.Poly.Internal.Dense as Dense
#if MIN_VERSION_semirings(0,4,2)
import Data.Poly.Internal.Dense.Fractional ()
import Data.Poly.Internal.Dense.GcdDomain ()
import Data.Poly.Internal.PolyOverFractional
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

#if MIN_VERSION_semirings(0,4,2)
-- | Execute the extended Euclidean algorithm.
-- For polynomials 'a' and 'b', compute their greatest common divisor 'g'
-- and the coefficient polynomial 's' satisfying 'a''s' + 'b''t' = 'g'.
--
-- >>> gcdExt (X^2 + 1 :: UPoly Double) (X^3 + 3 * X :: UPoly Double)
-- (1.0, 0.5 * X^2 + (-0.0) * X + 1.0)
-- >>> gcdExt (X^3 + 3 * X :: UPoly Double) (3 * X^4 + 3 * X^2 :: UPoly Double)
-- (3.0 * X + 0.0, (-0.5) * X^2 + (-0.0) * X + 1.0)
gcdExt :: (Eq (Poly v a), Euclidean (Poly v a), Ring (Poly v a))
  => Poly v a -> Poly v a -> (Poly v a, Poly v a)
gcdExt = Dense.gcdExt'
#endif
