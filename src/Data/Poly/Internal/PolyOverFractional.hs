-- |
-- Module:      Data.Poly.Internal.PolyOverFractional
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Wrapper with a more efficient 'Euclidean' instance.
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

#if MIN_VERSION_semirings(0,4,2)

module Data.Poly.Internal.PolyOverFractional
  ( PolyOverFractional(..)
  , fractionalGcdExt
  , scaleMonic
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm, (^))
import Control.DeepSeq (NFData)
import Data.Euclidean
import Data.Semiring
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G

import qualified Data.Poly.Internal.Dense as Dense
import qualified Data.Poly.Internal.Dense.Fractional as Dense (fractionalGcd, fractionalGcdExt, scaleMonic)

-- | Wrapper over polynomials,
-- providing a faster 'GcdDomain' instance,
-- when coefficients are 'Fractional'.
newtype PolyOverFractional poly = PolyOverFractional { unPolyOverFractional :: poly }
  deriving (Eq, NFData, Num, Ord, Semiring.Ring, Semiring, Show)

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

-- | Execute the extended Euclidean algorithm with fractional coefficients.
-- For polynomials 'a' and 'b', compute their unique greatest common divisor 'g'
-- and the unique coefficient polynomial 's' satisfying 'a''s' + 'b''t' = 'g',
-- such that either 'g' is monic, or 'g = 0' and 's' is monic, or 'g = s = 0'.
--
-- >>> fractionalGcdExt (X^2 + 1 :: UPoly Double) (X^3 + 3 * X :: UPoly Double)
-- (1.0, 0.5 * X^2 + (-0.0) * X + 1.0)
-- >>> fractionalGcdExt (X^3 + 3 * X :: UPoly Double) (3 * X^4 + 3 * X^2 :: UPoly Double)
-- (1.0 * X + 0.0,(-0.16666666666666666) * X^2 + (-0.0) * X + 0.3333333333333333)
fractionalGcdExt
  :: (Eq a, Fractional a, GcdDomain a, Semiring.Ring a, G.Vector v a, Eq (v a))
  => PolyOverFractional (Dense.Poly v a)
  -> PolyOverFractional (Dense.Poly v a)
  -> (PolyOverFractional (Dense.Poly v a), PolyOverFractional (Dense.Poly v a))
fractionalGcdExt (PolyOverFractional x) (PolyOverFractional y) =
  let (g, s) = Dense.fractionalGcdExt x y in
    (PolyOverFractional g, PolyOverFractional s)
{-# INLINE fractionalGcdExt #-}

-- | Scale a non-zero polynomial such that its leading coefficient is one,
-- returning the reciprocal of the leading coefficient in the scaling.
--
-- >>> scaleMonic (X^3 + 3 * X :: UPoly Double)
-- Just (1.0, 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0)
-- >>> scaleMonic (3 * X^4 + 3 * X^2 :: UPoly Double)
-- Just (0.3333333333333333, 1.0 * X^4 + 0.0 * X^3 + 1.0 * X^2 + 0.0 * X + 0.0)
scaleMonic
  :: (Eq a, Fractional a, GcdDomain a, Semiring.Ring a, G.Vector v a, Eq (v a))
  => PolyOverFractional (Dense.Poly v a)
  -> Maybe (a, PolyOverFractional (Dense.Poly v a))
scaleMonic (PolyOverFractional x) =
  (<$>) PolyOverFractional <$> Dense.scaleMonic x
{-# INLINE scaleMonic #-}

#else

module Data.Poly.Internal.PolyOverFractional () where

#endif
