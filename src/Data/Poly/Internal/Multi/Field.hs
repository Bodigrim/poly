-- |
-- Module:      Data.Poly.Internal.Multi.Field
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Euclidean for Field underlying
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Multi.Field () where

import Prelude hiding (quotRem, quot, rem, gcd)
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
    | otherwise = fromIntegral (SU.head (fst (G.unsafeLast xs)))

  quotRem = quotientRemainder

quotientRemainder
  :: (Eq a, Field a, G.Vector v (SU.Vector 1 Word, a))
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
            zs = MultiPoly $ G.singleton (SU.singleton (xp `minus` yp), xc `quot` yc)
            xs' = xs `minus` zs `times` ys
