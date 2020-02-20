-- |
-- Module:      Data.Poly.Internal.Dense.Field
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for Field underlying
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


{-# OPTIONS_GHC -fno-warn-orphans #-}

#if MIN_VERSION_semirings(0,4,2)

module Data.Poly.Internal.Dense.Field
  ( fieldGcd
  , Field
  ) where

import Prelude hiding (quotRem, quot, rem, gcd)
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean (Euclidean(..))
#if !MIN_VERSION_semirings(0,5,0)
import Data.Semiring (Ring)
#else
import Data.Euclidean (Field)
#endif
import Data.Semiring (times, minus, zero)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Data.Poly.Internal.Dense
import Data.Poly.Internal.Dense.GcdDomain ()

#if !MIN_VERSION_semirings(0,5,0)
type Field a = (Euclidean a, Ring a, Fractional a)
#endif

instance (Eq a, Eq (v a), Field a, G.Vector v a) => Euclidean (Poly v a) where
  degree (Poly xs) = fromIntegral (G.length xs)

  quotRem (Poly xs) (Poly ys) = (toPoly' qs, toPoly' rs)
    where
      (qs, rs) = quotientAndRemainder xs ys
  {-# INLINE quotRem #-}

  rem (Poly xs) (Poly ys) = toPoly' $ remainder xs ys
  {-# INLINE rem #-}

quotientAndRemainder
  :: (Field a, G.Vector v a)
  => v a
  -> v a
  -> (v a, v a)
quotientAndRemainder xs ys
  | G.null ys = throw DivideByZero
  | G.length xs < G.length ys = (G.empty, xs)
  | otherwise = runST $ do
    let lenXs = G.length xs
        lenYs = G.length ys
        lenQs = lenXs - lenYs + 1
    qs <- MG.unsafeNew lenQs
    rs <- MG.unsafeNew lenXs
    G.unsafeCopy rs xs
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead rs (lenYs - 1 + i)
      let q = r `quot` G.unsafeLast ys
      MG.unsafeWrite qs i q
      forM_ [0 .. lenYs - 1] $ \k -> do
        MG.unsafeModify rs (\c -> c `minus` q `times` G.unsafeIndex ys k) (i + k)
    let rs' = MG.unsafeSlice 0 lenYs rs
    (,) <$> G.unsafeFreeze qs <*> G.unsafeFreeze rs'
{-# INLINABLE quotientAndRemainder #-}

remainder
  :: (Field a, G.Vector v a)
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
  :: (PrimMonad m, Field a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m ()
remainderM xs ys
  | MG.null ys = throw DivideByZero
  | MG.length xs < MG.length ys = pure ()
  | otherwise = do
    let lenXs = MG.length xs
        lenYs = MG.length ys
        lenQs = lenXs - lenYs + 1
    yLast <- MG.unsafeRead ys (lenYs - 1)
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead xs (lenYs - 1 + i)
      forM_ [0 .. lenYs - 1] $ \k -> do
        y <- MG.unsafeRead ys k
        -- do not move r / yLast outside the loop,
        -- because of numerical instability
        MG.unsafeModify xs (\c -> c `minus` r `times` y `quot` yLast) (i + k)
{-# INLINABLE remainderM #-}

fieldGcd
  :: (Eq a, Field a, G.Vector v a)
  => Poly v a
  -> Poly v a
  -> Poly v a
fieldGcd (Poly xs) (Poly ys) = toPoly' $ runST $ do
  xs' <- G.thaw xs
  ys' <- G.thaw ys
  gcdM xs' ys'
{-# INLINE fieldGcd #-}

gcdM
  :: (PrimMonad m, Eq a, Field a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m (v a)
gcdM xs ys = do
  ys' <- dropWhileEndM (== zero) ys
  if MG.null ys' then G.unsafeFreeze xs else do
    remainderM xs ys'
    gcdM ys' xs
{-# INLINE gcdM #-}

#else

module Data.Poly.Internal.Dense.Field () where

#endif




