-- |
-- Module:      Data.Poly.Uni.Sparse.GcdDomain
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for GcdDomain underlying
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

module Data.Poly.Uni.Sparse.GcdDomain
  () where

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Data.Euclidean
import Data.Maybe
import Data.Semiring (Semiring(..))
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G

import Data.Poly.Uni.Sparse

-- | Consider using 'PolyOverFractional' wrapper,
-- which provides a much faster implementation of
-- 'Data.Euclidean.gcd' for 'Fractional'
-- coefficients.
instance (Eq a, Semiring.Ring a, GcdDomain a, Eq (v (Word, a)), G.Vector v (Word, a)) => GcdDomain (Poly v a) where
  divide xs ys = case leading ys of
    Nothing -> throw DivideByZero
    Just (yp, yc) -> case leading xs of
      Nothing -> Just xs
      Just (xp, xc)
        | xp < yp -> Nothing
        | otherwise -> do
          zc <- divide xc yc
          let z = Poly $ G.singleton (xp - yp, zc)
          rest <- divide (xs `plus` Semiring.negate z `times` ys) ys
          pure $ rest `plus` z

  gcd xs ys
    | G.null (unPoly xs) = ys
    | G.null (unPoly ys) = xs
    | otherwise = maybe err (times xy) (divide zs (constant' (cont zs)))
      where
        err = error "gcd: violated internal invariant"
        zs = gcdHelper xs ys
        cont ts = G.foldl' (\acc (_, t) -> gcd acc t) zero (unPoly ts)
        xy = constant' (gcd (cont xs) (cont ys))

gcdHelper
  :: (Eq a, Semiring.Ring a, GcdDomain a, G.Vector v (Word, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
gcdHelper xs ys = case leading xs of
  Nothing -> ys
  Just (xp, xc) -> case leading ys of
    Nothing -> xs
    Just (yp, yc) -> case xp `compare` yp of
      LT -> gcdHelper xs (ys `times` constant' gy `plus` Semiring.negate (xs `times` (Poly (G.singleton (yp - xp, gx)))))
      EQ -> gcdHelper xs (ys `times` constant' gy `plus` Semiring.negate (xs `times` constant' gx))
      GT -> gcdHelper (xs `times` constant' gx `plus` Semiring.negate (ys `times` (Poly (G.singleton (xp - yp, gy))))) ys
      where
        g = lcm xc yc
        gx = fromMaybe err $ divide g xc
        gy = fromMaybe err $ divide g yc
        err = error "gcd: violated internal invariant"

leading :: G.Vector v (Word, a) => Poly v a -> Maybe (Word, a)
leading (Poly xs)
  | G.null xs = Nothing
  | otherwise = Just $ G.last xs
