-- |
-- Module:      Data.Poly.Sparse.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Semiring' instance.
--

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Data.Poly.Sparse.Semiring
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , leading
  , toPoly
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

import Data.Euclidean (Field)
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G

import qualified Data.Poly.Internal.Convert as Convert
import qualified Data.Poly.Internal.Dense as Dense
import Data.Poly.Internal.Sparse (Poly(..), VPoly, UPoly, leading)
import qualified Data.Poly.Internal.Sparse as Sparse
import Data.Poly.Internal.Sparse.Field ()
import Data.Poly.Internal.Sparse.GcdDomain ()

-- | Make 'Poly' from a list of (power, coefficient) pairs.
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(1,2),(2,3)] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [(0,0),(1,0),(2,0)] :: UPoly Int
-- 0
toPoly :: (Eq a, Semiring a, G.Vector v (Word, a)) => v (Word, a) -> Poly v a
toPoly = Sparse.toPoly'

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Semiring a, G.Vector v (Word, a)) => Word -> a -> Poly v a
monomial = Sparse.monomial'

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 3 * X^2
scale :: (Eq a, Semiring a, G.Vector v (Word, a)) => Word -> a -> Poly v a -> Poly v a
scale = Sparse.scale'

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, G.Vector v (Word, a)) => Poly v a
pattern X = Sparse.X'

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
eval :: (Semiring a, G.Vector v (Word, a)) => Poly v a -> a -> a
eval = Sparse.eval'

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
subst :: (Eq a, Semiring a, G.Vector v (Word, a), G.Vector w (Word, a)) => Poly v a -> Poly w a -> Poly w a
subst = Sparse.subst'

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 3
deriv :: (Eq a, Semiring a, G.Vector v (Word, a)) => Poly v a -> Poly v a
deriv = Sparse.deriv'

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 3.0 * X
integral :: (Field a, G.Vector v (Word, a)) => Poly v a -> Poly v a
integral = Sparse.integral'

-- | Convert from dense to sparse polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> denseToSparse (1 `plus` Data.Poly.X^2) :: Data.Poly.Sparse.UPoly Int
-- 1 * X^2 + 1
denseToSparse :: (Eq a, Semiring a, G.Vector v a, G.Vector v (Word, a)) => Dense.Poly v a -> Sparse.Poly v a
denseToSparse = Convert.denseToSparse'

-- | Convert from sparse to dense polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> sparseToDense (1 `plus` Data.Poly.Sparse.X^2) :: Data.Poly.UPoly Int
-- 1 * X^2 + 0 * X + 1
sparseToDense :: (Semiring a, G.Vector v a, G.Vector v (Word, a)) => Sparse.Poly v a -> Dense.Poly v a
sparseToDense = Convert.sparseToDense'
