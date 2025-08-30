-- |
-- Module:      Data.Poly.Sparse.Laurent
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse
-- <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>.
--
-- @since 0.4.0.0

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE PatternSynonyms            #-}

{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-pattern-namespace-specifier #-}

module Data.Poly.Sparse.Laurent
  ( Laurent
  , VLaurent
  , ULaurent
  , unLaurent
  , toLaurent
  , leading
  , monomial
  , scale
  , pattern X
  , (^-)
  , eval
  , subst
  , deriv
  ) where

import Data.Euclidean (Field)
import Data.Semiring (Semiring(..), Ring)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Sized as SV

import Data.Poly.Internal.Multi.Laurent hiding (monomial, scale, pattern X, (^-), eval, subst, deriv)
import qualified Data.Poly.Internal.Multi.Laurent as Multi
import Data.Poly.Internal.Multi (Poly)

-- | Create a monomial from a power and a coefficient.
--
-- @since 0.4.0.0
monomial
  :: (Eq a, Semiring a, G.Vector v (Monom 1 a))
  => Int
  -> a
  -> Laurent v a
monomial = Multi.monomial . SU.singleton
{-# INLINABLE monomial #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^-2 + 1) :: ULaurent Int
-- 3 * X^2 + 3
--
-- @since 0.4.0.0
scale
  :: (Eq a, Semiring a, G.Vector v (Monom 1 a))
  => Int
  -> a
  -> Laurent v a
  -> Laurent v a
scale = Multi.scale . SU.singleton
{-# INLINABLE scale #-}

-- | The polynomial 'X'.
--
-- > X == monomial 1 one
--
-- @since 0.4.0.0
pattern X
  :: (Eq a, Semiring a, G.Vector v (Monom 1 a))
  => Laurent v a
pattern X = Multi.X

-- | Used to construct monomials with negative powers.
--
-- This operator can be applied only to monomials with unit coefficients,
-- but is instrumental to express Laurent polynomials
-- in a mathematical fashion:
--
-- >>> X^-3 :: ULaurent Int
-- 1 * X^-3
-- >>> X + 2 + 3 * (X^2)^-1 :: ULaurent Int
-- 1 * X + 2 + 3 * X^-2
--
-- @since 0.4.0.0
(^-)
  :: (Eq a, Semiring a, G.Vector v (Monom 1 a))
  => Laurent v a
  -> Int
  -> Laurent v a
(^-) = (Multi.^-)

-- | Evaluate the polynomial at a given point.
--
-- >>> eval (X^-2 + 1 :: ULaurent Double) 2
-- 1.25
--
-- @since 0.4.0.0
eval
  :: (Field a, G.Vector v (Monom 1 a))
  => Laurent v a
  -> a
  -> a
eval p = Multi.eval p . SV.singleton
{-# INLINABLE eval #-}

-- | Substitute another polynomial instead of 'X'.
--
-- >>> import Data.Poly.Sparse (UPoly)
-- >>> subst (Data.Poly.Sparse.X^2 + 1 :: UPoly Int) (X^-1 + 1 :: ULaurent Int)
-- 2 + 2 * X^-1 + 1 * X^-2
--
-- @since 0.4.0.0
subst
  :: (Eq a, Semiring a, G.Vector v (Monom 1 a), G.Vector w (Monom 1 a))
  => Poly v a
  -> Laurent w a
  -> Laurent w a
subst p = Multi.subst p . SV.singleton
{-# INLINABLE subst #-}

-- | Take the derivative of the polynomial.
--
-- >>> deriv (X^-3 + 3 * X) :: ULaurent Int
-- 3 + (-3) * X^-4
--
-- @since 0.4.0.0
deriv
  :: (Eq a, Ring a, G.Vector v (Monom 1 a))
  => Laurent v a
  -> Laurent v a
deriv = Multi.deriv 0
{-# INLINABLE deriv #-}
