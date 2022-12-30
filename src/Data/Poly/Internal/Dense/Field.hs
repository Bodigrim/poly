-- |
-- Module:      Data.Poly.Internal.Dense.Field
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'Euclidean' instance with a 'Field' constraint on the coefficient type.
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Dense.Field
  ( quotRemFractional
  ) where

import Prelude hiding (quotRem, quot, rem, gcd)
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean (Euclidean(..), Field)
import Data.Semiring (times, minus, zero, one)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Data.Poly.Internal.Dense
import Data.Poly.Internal.Dense.GcdDomain ()

-- | Note that 'degree' 0 = 0.
--
-- @since 0.3.0.0
instance (Eq a, Field a, G.Vector v a) => Euclidean (Poly v a) where
  degree (Poly xs)
    | G.null xs = 0
    | otherwise = fromIntegral (G.length xs - 1)

  quotRem (Poly xs) (Poly ys) = (toPoly' qs, toPoly' rs)
    where
      (qs, rs) = quotientAndRemainder zero (== one) minus times (one `quot`) xs ys
  {-# INLINE quotRem #-}

  rem (Poly xs) (Poly ys) = toPoly' $ remainder xs ys
  {-# INLINE rem #-}

-- | Polynomial division with remainder.
--
-- >>> quotRemFractional (X^3 + 2) (X^2 - 1 :: UPoly Double)
-- (1.0 * X + 0.0,1.0 * X + 2.0)
--
-- @since 0.5.0.0
quotRemFractional :: (Eq a, Fractional a, G.Vector v a) => Poly v a -> Poly v a -> (Poly v a, Poly v a)
quotRemFractional (Poly xs) (Poly ys) = (toPoly qs, toPoly rs)
  where
    (qs, rs) = quotientAndRemainder 0 (== 1) (-) (*) recip xs ys
{-# INLINE quotRemFractional #-}

quotientAndRemainder
  :: (Eq a, G.Vector v a)
  => a             -- ^ zero
  -> (a -> Bool)   -- ^ is one?
  -> (a -> a -> a) -- ^ subtract
  -> (a -> a -> a) -- ^ multiply
  -> (a -> a)      -- ^ invert
  -> v a           -- ^ dividend
  -> v a           -- ^ divisor
  -> (v a, v a)
quotientAndRemainder zer isOne sub mul inv xs ys
  | lenXs < lenYs = (G.empty, xs)
  | lenYs == 0 = throw DivideByZero
  | lenYs == 1 = let invY = inv (G.unsafeHead ys) in
                 (G.map (`mul` invY) xs, G.empty)
  | otherwise = runST $ do
    qs <- MG.unsafeNew lenQs
    rs <- MG.unsafeNew lenXs
    G.unsafeCopy rs xs
    let yLast = G.unsafeLast ys
        invYLast = inv yLast
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      r <- MG.unsafeRead rs (lenYs - 1 + i)
      let q = if isOne yLast then r else r `mul` invYLast
      MG.unsafeWrite qs i q
      MG.unsafeWrite rs (lenYs - 1 + i) zer
      forM_ [0 .. lenYs - 2] $ \k -> do
        let y = G.unsafeIndex ys k
        when (y /= zer) $
          MG.unsafeModify rs (\c -> c `sub` (q `mul` y)) (i + k)
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
    let invYLast = one `quot` yLast
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
