-- |
-- Module:      Data.Poly.Internal.Dense.Field
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for Field underlying
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Dense.Field () where

import Prelude hiding (quotRem, quot, rem, gcd, recip)
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean (Euclidean(..), Field)
import Data.Field (recip)
import Data.Semiring (times, minus, zero, one)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Data.Poly.Internal.Dense
import Data.Poly.Internal.Dense.GcdDomain ()

instance (Eq a, Eq (v a), Field a, G.Vector v a) => Euclidean (Poly v a) where
  degree (Poly xs) = fromIntegral (G.length xs)

  quotRem (Poly xs) (Poly ys) = (toPoly' qs, toPoly' rs)
    where
      (qs, rs) = quotientAndRemainder xs ys
  {-# INLINE quotRem #-}

  rem (Poly xs) (Poly ys) = toPoly' $ remainder xs ys
  {-# INLINE rem #-}

quotientAndRemainder
  :: (Eq a, Field a, G.Vector v a)
  => v a
  -> v a
  -> (v a, v a)
quotientAndRemainder xs ys
  | lenXs < lenYs = (G.empty, xs)
  | lenYs == 0 = throw DivideByZero
  | lenYs == 1 = let invY = recip (G.unsafeHead ys) in
                 (G.map (`times` invY) xs, G.empty)
  | otherwise = runST $ do
    qs <- MG.unsafeNew lenQs
    rs <- MG.unsafeNew lenXs
    G.unsafeCopy rs xs
    let yLast = G.unsafeLast ys
        invYLast = recip yLast
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead rs (lenYs - 1 + i)
      let q = if yLast == one then r else r `times` invYLast
      MG.unsafeWrite qs i q
      MG.unsafeWrite rs (lenYs - 1 + i) zero
      forM_ [0 .. lenYs - 2] $ \k -> do
        let y = G.unsafeIndex ys k
        when (y /= zero) $
          MG.unsafeModify rs (\c -> c `minus` q `times` y) (i + k)
    let rs' = MG.unsafeSlice 0 lenYs rs
    (,) <$> G.unsafeFreeze qs <*> G.unsafeFreeze rs'
  where
    lenXs = G.length xs
    lenYs = G.length ys
    lenQs = lenXs - lenYs + 1
{-# INLINABLE quotientAndRemainder #-}

remainder
  :: (Eq a, Field a, G.Vector v a)
  => v a
  -> v a
  -> v a
remainder xs ys
  | G.null ys = throw DivideByZero
  | otherwise = runST $ do
    rs <- G.thaw xs
    ys' <- G.unsafeThaw ys
    remainderM rs ys'
    G.unsafeFreeze $ MG.unsafeSlice 0 (G.length xs `min` G.length ys) rs
{-# INLINABLE remainder #-}

remainderM
  :: (PrimMonad m, Eq a, Field a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m ()
remainderM xs ys
  | lenXs < lenYs = pure ()
  | lenYs == 0 = throw DivideByZero
  | lenYs == 1 = MG.set xs zero
  | otherwise = do
    yLast <- MG.unsafeRead ys (lenYs - 1)
    let invYLast = recip yLast
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead xs (lenYs - 1 + i)
      MG.unsafeWrite xs (lenYs - 1 + i) zero
      let q = if yLast == one then r else r `times` invYLast
      forM_ [0 .. lenYs - 2] $ \k -> do
        y <- MG.unsafeRead ys k
        when (y /= zero) $
          MG.unsafeModify xs (\c -> c `minus` q `times` y) (i + k)
  where
    lenXs = MG.length xs
    lenYs = MG.length ys
    lenQs = lenXs - lenYs + 1
{-# INLINABLE remainderM #-}
