-- |
-- Module:      Data.Poly.Internal.Sparse.Fractional
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for Fractional underlying
--

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Sparse.Fractional
  ( fractionalGcd
  ) where

import Prelude hiding (quotRem, rem, gcd)
import Control.Arrow
import Control.Exception
import Data.Euclidean
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Sparse
import Data.Poly.Internal.Sparse.GcdDomain ()

instance (Eq a, Eq (v (Word, a)), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v (Word, a)) => Euclidean (Poly v a) where
  degree (Poly xs)
    | G.null xs = 0
    | otherwise = 1 + fromIntegral (fst (G.last xs))

  quotRem = quotientRemainder

quotientRemainder
  :: (Eq a, Fractional a, G.Vector v (Word, a))
  => Poly v a
  -> Poly v a
  -> (Poly v a, Poly v a)
quotientRemainder ts ys = case leading ys of
  Nothing -> throw DivideByZero
  Just (yp, yc) -> go ts
    where
      go xs = case leading xs of
        Nothing -> (0, 0)
        Just (xp, xc) -> case xp `compare` yp of
          LT -> (0, xs)
          EQ -> (zs, xs')
          GT -> first (+ zs) $ go xs'
          where
            zs = Poly $ G.singleton (xp - yp, xc / yc)
            xs' = xs - zs * ys

fractionalGcd
  :: (Eq a, Fractional a, G.Vector v (Word, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
fractionalGcd xs ys
  | G.null (unPoly ys) = xs
  | otherwise = fractionalGcd ys $ snd $ quotientRemainder xs ys
{-# INLINE fractionalGcd #-}

leading :: G.Vector v (Word, a) => Poly v a -> Maybe (Word, a)
leading (Poly xs)
  | G.null xs = Nothing
  | otherwise = Just $ G.last xs
