-- |
-- Module:      Data.Poly.Sparse
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Num' instance.
--

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Data.Poly.Sparse
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , toPoly
  , leading
  , monomial
  , scale
  , pattern X
  , eval
  , subst
  , deriv
  , integral
  , denseToSparse
  , sparseToDense
  ) where

import Control.Arrow
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Sized as SV

import Data.Poly.Internal.Convert
import Data.Poly.Internal.Multi (Poly, VPoly, UPoly, unPoly, leading)
import qualified Data.Poly.Internal.Multi as Multi
import Data.Poly.Internal.Multi.Field ()
import Data.Poly.Internal.Multi.GcdDomain ()

-- | Make 'Poly' from a list of (power, coefficient) pairs.
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(1,2),(2,3)] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [(0,0),(1,0),(2,0)] :: UPoly Int
-- 0
toPoly
  :: (Eq a, Num a, G.Vector v (Word, a), G.Vector v (SU.Vector 1 Word, a))
  => v (Word, a)
  -> Poly v a
toPoly = Multi.toMultiPoly . G.map (first SU.singleton)

-- | Create a monomial from a power and a coefficient.
monomial
  :: (Eq a, Num a, G.Vector v (SU.Vector 1 Word, a))
  => Word
  -> a
  -> Poly v a
monomial = Multi.monomial . SU.singleton

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 3 * X^2
scale
  :: (Eq a, Num a, G.Vector v (SU.Vector 1 Word, a))
  => Word
  -> a
  -> Poly v a
  -> Poly v a
scale = Multi.scale . SU.singleton

-- | Create an identity polynomial.
pattern X
  :: (Eq a, Num a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
pattern X = Multi.X

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
eval
  :: (Num a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> a
  -> a
eval p = Multi.eval p . SV.singleton

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
subst
  :: (Eq a, Num a, G.Vector v (SU.Vector 1 Word, a), G.Vector w (SU.Vector 1 Word, a))
  => Poly v a
  -> Poly w a
  -> Poly w a
subst p = Multi.subst p . SV.singleton

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 3
deriv
  :: (Eq a, Num a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> Poly v a
deriv = Multi.deriv 0

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 3.0 * X
integral
  :: (Fractional a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> Poly v a
integral = Multi.integral 0
