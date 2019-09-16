-- |
-- Module:      Data.Poly.Internal.Sparse.GcdDomain
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for GcdDomain underlying
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Sparse.GcdDomain
  () where

#if MIN_VERSION_semirings(0,4,2)

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Data.Euclidean
import Data.Maybe
import Data.Semiring (Semiring(..), Ring(), minus)
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Sparse

-- | Consider using 'Data.Poly.Sparse.Semiring.PolyOverField' wrapper,
-- which provides a much faster implementation of
-- 'Data.Euclidean.gcd' for polynomials over 'Field'.
instance (Eq a, Ring a, GcdDomain a, Eq (v (Word, a)), G.Vector v (Word, a)) => GcdDomain (Poly v a) where
  divide xs ys = case leading ys of
    Nothing -> throw DivideByZero
    Just (yp, yc) -> case leading xs of
      Nothing -> Just xs
      Just (xp, xc)
        | xp < yp -> Nothing
        | otherwise -> do
          zc <- divide xc yc
          let z = Poly $ G.singleton (xp - yp, zc)
          rest <- divide (xs `minus` z `times` ys) ys
          pure $ rest `plus` z

  gcd xs ys
    | G.null (unPoly xs) = ys
    | G.null (unPoly ys) = xs
    | otherwise = maybe err (times xy) (divide zs (monomial' 0 (cont zs)))
      where
        err = error "gcd: violated internal invariant"
        zs = gcdHelper xs ys
        cont ts = G.foldl' (\acc (_, t) -> gcd acc t) zero (unPoly ts)
        xy = monomial' 0 (gcd (cont xs) (cont ys))

gcdHelper
  :: (Eq a, Ring a, GcdDomain a, G.Vector v (Word, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
gcdHelper xs ys = case leading xs of
  Nothing -> ys
  Just (xp, xc) -> case leading ys of
    Nothing -> xs
    Just (yp, yc) -> case xp `compare` yp of
      LT -> gcdHelper xs (ys `times` monomial' 0 gy `minus` xs `times` monomial' (yp - xp) gx)
      EQ -> gcdHelper xs (ys `times` monomial' 0 gy `minus` xs `times` monomial' 0 gx)
      GT -> gcdHelper (xs `times` monomial' 0 gx `minus` ys `times` monomial' (xp - yp) gy) ys
      where
        g = lcm xc yc
        gx = fromMaybe err $ divide g xc
        gy = fromMaybe err $ divide g yc
        err = error "gcd: violated internal invariant"

#endif
