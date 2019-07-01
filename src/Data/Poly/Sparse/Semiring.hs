-- |
-- Module:      Data.Poly.Sparse.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Semiring' instance.
--

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}

module Data.Poly.Sparse.Semiring
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
  -- * Fractional coefficients
  , PolyOverFractional(..)
  ) where

import Data.Semiring (Semiring)
import qualified Data.Vector.Generic as G

import Data.Poly.Uni.Sparse (Poly(..), VPoly, UPoly)
import qualified Data.Poly.Uni.Sparse as Sparse
import Data.Poly.Uni.Sparse.Fractional ()
import Data.Poly.Uni.Sparse.GcdDomain ()
import Data.Poly.Uni.PolyOverFractional

-- | Make 'Poly' from a list of (power, coefficient) pairs.
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(1,2),(2,3)] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> S.toPoly [(0,0),(1,0),(2,0)] :: UPoly Int
-- 0
toPoly :: (Eq a, Semiring a, G.Vector v (Word, a)) => v (Word, a) -> Poly v a
toPoly = Sparse.toPoly'

-- | Create a polynomial from a constant term.
constant :: (Eq a, Semiring a, G.Vector v (Word, a)) => a -> Poly v a
constant = Sparse.constant'

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, G.Vector v (Word, a), Eq (v (Word, a))) => Poly v a
pattern X = Sparse.X'

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Semiring a, G.Vector v (Word, a)) => Poly v a -> a -> a
eval = Sparse.eval'

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 3
deriv :: (Eq a, Semiring a, G.Vector v (Word, a)) => Poly v a -> Poly v a
deriv = Sparse.deriv'
