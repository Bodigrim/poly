-- |
-- Module:      Data.Poly.Internal.Multi.Field
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Euclidean' instance with a 'Field' constraint on the coefficient type.
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Multi.Field
  ( quotRemFractional
  ) where

import Prelude hiding (quotRem, quot, rem, div, gcd)
import Control.Arrow
import Control.Exception
import Data.Euclidean (Euclidean(..), Field)
import Data.Semiring (Semiring(..), minus)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU

import Data.Poly.Internal.Multi
import Data.Poly.Internal.Multi.GcdDomain ()

-- | Note that 'degree' 0 = 0.
instance (Eq a, Field a, G.Vector v (SU.Vector 1 Word, a)) => Euclidean (Poly v a) where
  degree (MultiPoly xs)
    | G.null xs = 0
    | otherwise = fromIntegral (SU.head (fst (G.last xs)))

  quotRem = quotientRemainder zero plus minus times quot

-- | Polynomial division with remainder.
--
-- >>> quotRemFractional (X^3 + 2) (X^2 - 1 :: UPoly Double)
-- (1.0 * X,1.0 * X + 2.0)
--
-- @since 0.5.0.0
quotRemFractional :: (Eq a, Fractional a, G.Vector v (SU.Vector 1 Word, a)) => Poly v a -> Poly v a -> (Poly v a, Poly v a)
quotRemFractional = quotientRemainder 0 (+) (-) (*) (/)
{-# INLINE quotRemFractional #-}

quotientRemainder
  :: G.Vector v (SU.Vector 1 Word, a)
  => Poly v a                           -- ^ zero
  -> (Poly v a -> Poly v a -> Poly v a) -- ^ add
  -> (Poly v a -> Poly v a -> Poly v a) -- ^ subtract
  -> (Poly v a -> Poly v a -> Poly v a) -- ^ multiply
  -> (a -> a -> a)                      -- ^ divide
  -> Poly v a                           -- ^ dividend
  -> Poly v a                           -- ^ divisor
  -> (Poly v a, Poly v a)
quotientRemainder zer add sub mul div ts ys = case leading ys of
  Nothing -> throw DivideByZero
  Just (yp, yc) -> go ts
    where
      go xs = case leading xs of
        Nothing -> (zer, zer)
        Just (xp, xc) -> case xp `compare` yp of
          LT -> (zer, xs)
          EQ -> (zs, xs')
          GT -> first (`add` zs) $ go xs'
          where
            zs = MultiPoly $ G.singleton (SU.singleton (xp - yp), xc `div` yc)
            xs' = xs `sub` (zs `mul` ys)
