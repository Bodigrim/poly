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
  , gcdExt
  ) where

import Prelude hiding (quotRem, quot, rem, gcd)
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean
#if !MIN_VERSION_semirings(0,5,0)
import Data.Semiring (Ring)
#endif
import Data.Semiring (times, minus, zero, one)
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
{-# INLINE quotientAndRemainder #-}

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
{-# INLINE remainder #-}

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
{-# INLINE remainderM #-}

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

-- | Execute the extended Euclidean algorithm.
-- For polynomials @a@ and @b@, compute their unique greatest common divisor @g@
-- and the unique coefficient polynomial @s@ satisfying @as + bt = g@,
-- such that either @g@ is monic, or @g = 0@ and @s@ is monic, or @g = s = 0@.
--
-- >>> gcdExt (X^2 + 1 :: UPoly Double) (X^3 + 3 * X :: UPoly Double)
-- (1.0, 0.5 * X^2 + (-0.0) * X + 1.0)
-- >>> gcdExt (X^3 + 3 * X :: UPoly Double) (3 * X^4 + 3 * X^2 :: UPoly Double)
-- (1.0 * X + 0.0,(-0.16666666666666666) * X^2 + (-0.0) * X + 0.3333333333333333)
gcdExt
  :: (Eq a, Field a, G.Vector v a, Eq (v a))
  => Poly v a
  -> Poly v a
  -> (Poly v a, Poly v a)
gcdExt xs ys = case scaleMonic gs of
  Just (c', gs') -> (gs', scale' zero c' ss)
  Nothing -> case scaleMonic ss of
    Just (_, ss') -> (zero, ss')
    Nothing -> (zero, zero)
  where
    (gs, ss) = go ys xs zero one
      where
        go r' r s' s
          | r' == zero = (r, s)
          | otherwise  = case r `quotRem` r' of
            (q, r'') -> go r'' r' (s `minus` q `times` s') s'
{-# INLINE gcdExt #-}

-- | Scale a non-zero polynomial such that its leading coefficient is one,
-- returning the reciprocal of the leading coefficient in the scaling.
--
-- >>> scaleMonic (X^3 + 3 * X :: UPoly Double)
-- Just (1.0, 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0)
-- >>> scaleMonic (3 * X^4 + 3 * X^2 :: UPoly Double)
-- Just (0.3333333333333333, 1.0 * X^4 + 0.0 * X^3 + 1.0 * X^2 + 0.0 * X + 0.0)
scaleMonic
  :: (Eq a, Field a, G.Vector v a, Eq (v a))
  => Poly v a
  -> Maybe (a, Poly v a)
scaleMonic xs = case leading xs of
  Nothing -> Nothing
  Just (_, c) -> let c' = one `quot` c in Just (c', scale' zero c' xs)
{-# INLINE scaleMonic #-}

#else

module Data.Poly.Internal.Dense.Field () where

#endif
