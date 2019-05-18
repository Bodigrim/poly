-- |
-- Module:      Data.Poly.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Semiring'-based interface.
--

{-# LANGUAGE PatternSynonyms     #-}

module Data.Poly.Semiring
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  -- * Semiring interface
  , toPoly
  , constant
  , pattern X
  , eval
  , deriv
  ) where

import Data.Semiring (Semiring)
import qualified Data.Vector.Generic as G

import Data.Poly.Uni.Dense (Poly(..), VPoly, UPoly)
import qualified Data.Poly.Uni.Dense as Dense
import Data.Poly.Uni.Dense.Fractional ()
import Data.Poly.Uni.Dense.GcdDomain ()

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

-- | Create a polynomial from a constant term.
constant :: (Eq a, Semiring a, G.Vector v a) => a -> Poly v a
constant = Dense.constant'

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
