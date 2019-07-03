-- |
-- Module:      Data.Poly.Internal.Dense.Fractional
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for Fractional underlying
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Dense.Fractional
  ( fractionalGcd
  ) where

import Prelude hiding (rem, gcd)
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Data.Poly.Internal.Dense
import Data.Poly.Internal.Dense.GcdDomain ()

instance (Eq a, Eq (v a), Semiring.Ring a, GcdDomain a, Fractional a, G.Vector v a) => Euclidean (Poly v a) where
  degree (Poly xs) = fromIntegral (G.basicLength xs)

  quotRem (Poly xs) (Poly ys) = (toPoly qs, toPoly rs)
    where
      (qs, rs) = quotientAndRemainder xs ys
  {-# INLINE quotRem #-}

  rem (Poly xs) (Poly ys) = toPoly $ remainder xs ys
  {-# INLINE rem #-}

quotientAndRemainder
  :: (Fractional a, G.Vector v a)
  => v a
  -> v a
  -> (v a, v a)
quotientAndRemainder xs ys
  | G.null ys = throw DivideByZero
  | G.basicLength xs < G.basicLength ys = (G.empty, xs)
  | otherwise = runST $ do
    let lenXs = G.basicLength xs
        lenYs = G.basicLength ys
        lenQs = lenXs - lenYs + 1
    qs <- MG.basicUnsafeNew lenQs
    rs <- MG.basicUnsafeNew lenXs
    G.unsafeCopy rs xs
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead rs (lenYs - 1 + i)
      let q = r / G.unsafeLast ys
      MG.unsafeWrite qs i q
      forM_ [0 .. lenYs - 1] $ \k -> do
        MG.unsafeModify rs (\c -> c - q * G.unsafeIndex ys k) (i + k)
    let rs' = MG.basicUnsafeSlice 0 lenYs rs
    (,) <$> G.unsafeFreeze qs <*> G.unsafeFreeze rs'
{-# INLINE quotientAndRemainder #-}

remainder
  :: (Fractional a, G.Vector v a)
  => v a
  -> v a
  -> v a
remainder xs ys
  | G.null ys = throw DivideByZero
  | otherwise = runST $ do
    rs <- G.thaw xs
    ys' <- G.unsafeThaw ys
    remainderM rs ys'
    G.unsafeFreeze $ MG.basicUnsafeSlice 0 (G.basicLength xs `min` G.basicLength ys) rs
{-# INLINE remainder #-}

remainderM
  :: (PrimMonad m, Fractional a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m ()
remainderM xs ys
  | MG.null ys = throw DivideByZero
  | MG.basicLength xs < MG.basicLength ys = pure ()
  | otherwise = do
    let lenXs = MG.basicLength xs
        lenYs = MG.basicLength ys
        lenQs = lenXs - lenYs + 1
    yLast <- MG.unsafeRead ys (lenYs - 1)
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead xs (lenYs - 1 + i)
      forM_ [0 .. lenYs - 1] $ \k -> do
        y <- MG.unsafeRead ys k
        -- do not move r / yLast outside the loop,
        -- because of numerical instability
        MG.unsafeModify xs (\c -> c - r * y / yLast) (i + k)
{-# INLINE remainderM #-}

fractionalGcd
  :: (Eq a, Fractional a, G.Vector v a)
  => Poly v a
  -> Poly v a
  -> Poly v a
fractionalGcd (Poly xs) (Poly ys) = toPoly $ runST $ do
  xs' <- G.thaw xs
  ys' <- G.thaw ys
  gcdM xs' ys'
{-# INLINE fractionalGcd #-}

gcdM
  :: (PrimMonad m, Eq a, Fractional a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m (v a)
gcdM xs ys = do
  ys' <- dropWhileEndM (== 0) ys
  if MG.null ys' then G.unsafeFreeze xs else do
    remainderM xs ys'
    gcdM ys' xs
{-# INLINE gcdM #-}
