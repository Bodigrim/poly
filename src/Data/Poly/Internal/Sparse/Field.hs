-- |
-- Module:      Data.Poly.Internal.Sparse.Field
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for Field underlying
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

#if MIN_VERSION_semirings(0,4,2)

module Data.Poly.Internal.Sparse.Field
  ( gcdExt
  ) where

import Prelude hiding (quotRem, quot, rem, gcd)
import Control.Arrow
import Control.Exception
import Data.Euclidean
#if !MIN_VERSION_semirings(0,5,0)
import Data.Semiring (Ring)
#endif
import Data.Semiring (minus, plus, times, zero, one)
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Sparse
import Data.Poly.Internal.Sparse.GcdDomain ()

#if !MIN_VERSION_semirings(0,5,0)
type Field a = (Euclidean a, Ring a, Fractional a)
#endif

instance (Eq a, Eq (v (Word, a)), Field a, G.Vector v (Word, a)) => Euclidean (Poly v a) where
  degree (Poly xs)
    | G.null xs = 0
    | otherwise = 1 + fromIntegral (fst (G.last xs))

  quotRem = quotientRemainder

quotientRemainder
  :: (Eq a, Field a, G.Vector v (Word, a))
  => Poly v a
  -> Poly v a
  -> (Poly v a, Poly v a)
quotientRemainder ts ys = case leading ys of
  Nothing -> throw DivideByZero
  Just (yp, yc) -> go ts
    where
      go xs = case leading xs of
        Nothing -> (zero, zero)
        Just (xp, xc) -> case xp `compare` yp of
          LT -> (zero, xs)
          EQ -> (zs, xs')
          GT -> first (`plus` zs) $ go xs'
          where
            zs = Poly $ G.singleton (xp `minus` yp, xc `quot` yc)
            xs' = xs `minus` zs `times` ys

-- | Execute the extended Euclidean algorithm.
-- For polynomials @a@ and @b@, compute their unique greatest common divisor @g@
-- and the unique coefficient polynomial @s@ satisfying @as + bt = g@,
-- such that either @g@ is monic, or @g = 0@ and @s@ is monic, or @g = s = 0@.
--
-- >>> gcdExt (X^2 + 1 :: UPoly Double) (X^3 + 3 * X :: UPoly Double)
-- (1.0, 0.5 * X^2 + (-0.0) * X + 1.0)
-- >>> gcdExt (X^3 + 3 * X :: UPoly Double) (3 * X^4 + 3 * X^2 :: UPoly Double)
-- (1.0 * X + 0.0,(-0.16666666666666666) * X^2 + (-0.0) * X + 0.3333333333333333)
gcdExt
  :: (Eq a, Field a, G.Vector v (Word, a), Eq (v (Word, a)))
  => Poly v a
  -> Poly v a
  -> (Poly v a, Poly v a)
gcdExt xs ys = case scaleMonic gs of
  Just (c', gs') -> (gs', scale' zero c' ss)
  Nothing -> case scaleMonic ss of
    Just (_, ss') -> (zero, ss')
    Nothing -> (zero, zero)
  where
    (gs, ss) = go ys xs zero one
      where
        go r' r s' s
          | r' == zero = (r, s)
          | otherwise  = case r `quotRem` r' of
            (q, r'') -> go r'' r' (s `minus` q `times` s') s'
{-# INLINE gcdExt #-}

-- | Scale a non-zero polynomial such that its leading coefficient is one,
-- returning the reciprocal of the leading coefficient in the scaling.
--
-- >>> scaleMonic (X^3 + 3 * X :: UPoly Double)
-- Just (1.0, 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0)
-- >>> scaleMonic (3 * X^4 + 3 * X^2 :: UPoly Double)
-- Just (0.3333333333333333, 1.0 * X^4 + 0.0 * X^3 + 1.0 * X^2 + 0.0 * X + 0.0)
scaleMonic
  :: (Eq a, Field a, G.Vector v (Word, a), Eq (v (Word, a)))
  => Poly v a
  -> Maybe (a, Poly v a)
scaleMonic xs = case leading xs of
  Nothing -> Nothing
  Just (_, c) -> let c' = one `quot` c in Just (c', scale' zero c' xs)
{-# INLINE scaleMonic #-}

#else

module Data.Poly.Internal.Sparse.Field () where

#endif
