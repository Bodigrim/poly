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

module Data.Poly.Internal.Sparse.Field () where

import Prelude hiding (quotRem, quot, rem, gcd)
import Control.Arrow
import Control.Exception
import Data.Euclidean (Euclidean(..))
#if !MIN_VERSION_semirings(0,5,0)
import Data.Semiring (Ring)
#else
import Data.Euclidean (Field)
#endif
import Data.Semiring (minus, plus, times, zero)
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

#else

module Data.Poly.Internal.Sparse.Field () where

#endif
