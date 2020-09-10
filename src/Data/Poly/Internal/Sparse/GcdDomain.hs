-- |
-- Module:      Data.Poly.Internal.Sparse.GcdDomain
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for GcdDomain underlying
--

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Sparse.GcdDomain
  () where

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Data.Euclidean
import Data.Maybe
import Data.Semiring (Semiring(..), Ring(), minus)
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Sparse

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
    | G.length (unPoly xs) == 1 = gcdSingleton (G.unsafeHead (unPoly xs)) ys
    | G.length (unPoly ys) == 1 = gcdSingleton (G.unsafeHead (unPoly ys)) xs
    | otherwise = times xy (divide' zs (monomial' 0 (content zs)))
      where
        zs = gcdHelper xs ys
        xy = monomial' 0 (gcd (content xs) (content ys))

content :: (Eq a, GcdDomain a, G.Vector v (Word, a)) => Poly v a -> a
content (Poly ts) = G.foldl' (\acc (_, t) -> gcd acc t) zero ts

gcdSingleton
  :: (Eq a, GcdDomain a, G.Vector v (Word, a))
  => (Word, a)
  -> Poly v a
  -> Poly v a
gcdSingleton (p, c) pcs = monomial'
  (min p (fst (G.unsafeHead (unPoly pcs))))
  (gcd c (content pcs))

gcdHelper
  :: (Eq a, Ring a, GcdDomain a, G.Vector v (Word, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
gcdHelper xs ys = case (leading xs, leading ys) of
  (Nothing, _) -> ys
  (_, Nothing) -> xs
  (Just (xp, xc), Just (yp, yc))
    | yp <= xp
    , Just xy <- xc `divide` yc
    -> gcdHelper ys (xs `minus` ys `times` monomial' (xp - yp) xy)
    | xp <= yp
    , Just yx <- yc `divide` xc
    -> gcdHelper xs (ys `minus` xs `times` monomial' (yp - xp) yx)
    | yp <= xp
    -> gcdHelper ys (xs `times` monomial' 0 gx `minus` ys `times` monomial' (xp - yp) gy)
    | otherwise
    -> gcdHelper xs (ys `times` monomial' 0 gy `minus` xs `times` monomial' (yp - xp) gx)
    where
      g = lcm xc yc
      gx = divide' g xc
      gy = divide' g yc

divide' :: GcdDomain a => a -> a -> a
divide' = (fromMaybe (error "gcd: violated internal invariant") .) . divide
