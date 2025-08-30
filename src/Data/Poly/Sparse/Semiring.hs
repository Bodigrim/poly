-- |
-- Module:      Data.Poly.Sparse.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with a 'Semiring' instance.
--
-- @since 0.3.0.0
--

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-pattern-namespace-specifier #-}

module Data.Poly.Sparse.Semiring
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

import Data.Euclidean (Field)
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Sized as SV

import qualified Data.Poly.Internal.Convert as Convert
import qualified Data.Poly.Internal.Dense as Dense
import Data.Poly.Internal.Multi (Poly, VPoly, UPoly, unPoly, leading)
import qualified Data.Poly.Internal.Multi as Multi
import Data.Poly.Internal.Multi.Field ()
import Data.Poly.Internal.Multi.GcdDomain ()

-- | Make a 'Poly' from a list of (power, coefficient) pairs.
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [(0,1),(1,2),(2,3)] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [(0,0),(1,0),(2,0)] :: UPoly Int
-- 0
--
-- @since 0.3.0.0
toPoly
  :: (Eq a, Semiring a, G.Vector v (Word, a), G.Vector v (Multi.Monom 1 a))
  => v (Word, a)
  -> Poly v a
toPoly = Multi.toMultiPoly' . G.map (\(p, c) -> Multi.Monom (SU.singleton p) c)
{-# INLINABLE toPoly #-}

-- | Create a monomial from a power and a coefficient.
--
-- @since 0.3.0.0
monomial
  :: (Eq a, Semiring a, G.Vector v (Multi.Monom 1 a))
  => Word
  -> a
  -> Poly v a
monomial = Multi.monomial' . SU.singleton
{-# INLINABLE monomial #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 3 * X^2
--
-- @since 0.3.0.0
scale
  :: (Eq a, Semiring a, G.Vector v (Multi.Monom 1 a))
  => Word
  -> a
  -> Poly v a
  -> Poly v a
scale = Multi.scale' . SU.singleton
{-# INLINABLE scale #-}

-- | The polynomial 'X'.
--
-- > X == monomial 1 one
--
-- @since 0.3.0.0
pattern X
  :: (Eq a, Semiring a, G.Vector v (Multi.Monom 1 a))
  => Poly v a
pattern X = Multi.X'

-- | Evaluate the polynomial at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
--
-- @since 0.3.0.0
eval
  :: (Semiring a, G.Vector v (Multi.Monom 1 a))
  => Poly v a
  -> a
  -> a
eval p = Multi.eval' p . SV.singleton
{-# INLINABLE eval #-}

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
--
-- @since 0.3.3.0
subst
  :: (Eq a, Semiring a, G.Vector v (Multi.Monom 1 a), G.Vector w (Multi.Monom 1 a))
  => Poly v a
  -> Poly w a
  -> Poly w a
subst p = Multi.subst' p . SV.singleton
{-# INLINABLE subst #-}

-- | Take the derivative of the polynomial.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 3
--
-- @since 0.3.0.0
deriv
  :: (Eq a, Semiring a, G.Vector v (Multi.Monom 1 a))
  => Poly v a
  -> Poly v a
deriv = Multi.deriv' 0
{-# INLINABLE deriv #-}

-- | Compute an indefinite integral of the polynomial,
-- setting the constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 3.0 * X
--
-- @since 0.3.2.0
integral
  :: (Field a, G.Vector v (Multi.Monom 1 a))
  => Poly v a
  -> Poly v a
integral = Multi.integral' 0
{-# INLINABLE integral #-}

-- | Convert from dense to sparse polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> denseToSparse (1 `Data.Semiring.plus` Data.Poly.X^2) :: Data.Poly.Sparse.UPoly Int
-- 1 * X^2 + 1
--
-- @since 0.5.0.0
denseToSparse
  :: (Eq a, Semiring a, G.Vector v a, G.Vector v (Multi.Monom 1 a))
  => Dense.Poly v a
  -> Multi.Poly v a
denseToSparse = Convert.denseToSparse'
{-# INLINABLE denseToSparse #-}

-- | Convert from sparse to dense polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> sparseToDense (1 `Data.Semiring.plus` Data.Poly.Sparse.X^2) :: Data.Poly.UPoly Int
-- 1 * X^2 + 0 * X + 1
--
-- @since 0.5.0.0
sparseToDense
  :: (Semiring a, G.Vector v a, G.Vector v (Multi.Monom 1 a))
  => Multi.Poly v a
  -> Dense.Poly v a
sparseToDense = Convert.sparseToDense'
{-# INLINABLE sparseToDense #-}
