-- |
-- Module:      Data.Poly.Internal.Dense.GcdDomain
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- 'GcdDomain' instance with a 'GcdDomain' constaint on the coefficient type.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Dense.GcdDomain
  () where

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Euclidean
import Data.Maybe
import Data.Semiring (Semiring(..), Ring(), isZero, minus)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

import Data.Poly.Internal.Dense

instance (Eq a, Ring a, GcdDomain a, G.Vector v a) => GcdDomain (Poly v a) where
  divide (Poly xs) (Poly ys) =
    toPoly' <$> quotient xs ys

  gcd (Poly xs) (Poly ys)
    | G.null xs = Poly ys
    | G.null ys = Poly xs
    | G.length xs == 1 = Poly $ G.singleton $ G.foldl' gcd (G.unsafeHead xs) ys
    | G.length ys == 1 = Poly $ G.singleton $ G.foldl' gcd (G.unsafeHead ys) xs
    | otherwise = toPoly' $ gcdNonEmpty xs ys
  {-# INLINE gcd #-}

  lcm x@(Poly xs) y@(Poly ys)
    | G.null xs || G.null ys = zero
    | otherwise = (x `divide'` gcd x y) `times` y

  coprime x y = isJust (one `divide` gcd x y)

gcdNonEmpty
  :: (Eq a, Ring a, GcdDomain a, G.Vector v a)
  => v a
  -> v a
  -> v a
gcdNonEmpty xs ys = runST $ do
    let x = G.foldl1' gcd xs
        y = G.foldl1' gcd ys
        xy = x `gcd` y
    xs' <- G.thaw xs
    ys' <- G.thaw ys
    zs' <- gcdM xs' ys'

    let lenZs = MG.length zs'
        go acc 0 = pure acc
        go acc n = do
          t <- MG.unsafeRead zs' (n - 1)
          go (acc `gcd` t) (n - 1)
    a <- MG.unsafeRead zs' (lenZs - 1)
    z <- go a (lenZs - 1)

    forM_ [0 .. lenZs - 1] $ \i ->
      MG.unsafeModify zs'((`times` xy) . (`divide'` z)) i

    G.unsafeFreeze zs'
{-# INLINABLE gcdNonEmpty #-}

gcdM
  :: (PrimMonad m, Eq a, Ring a, GcdDomain a, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> G.Mutable v (PrimState m) a
  -> m (G.Mutable v (PrimState m) a)
gcdM xs ys
  | MG.null xs = pure ys
  | MG.null ys = pure xs
  | otherwise = do
  let lenXs = MG.length xs
      lenYs = MG.length ys
  xLast <- MG.unsafeRead xs (lenXs - 1)
  yLast <- MG.unsafeRead ys (lenYs - 1)
  let z  = xLast `lcm` yLast
      zx = z `divide'` xLast
      zy = z `divide'` yLast

  if
    | lenYs <= lenXs
    , Just xy <- xLast `divide` yLast -> do
      forM_ [0 .. lenYs - 1] $ \i -> do
        y <- MG.unsafeRead ys i
        when (y /= zero) $
          MG.unsafeModify
            xs
            (\x -> x `minus` y `times` xy)
            (i + lenXs - lenYs)
      xs' <- dropWhileEndM isZero xs
      gcdM xs' ys
    | lenXs <= lenYs
    , Just yx <- yLast `divide` xLast -> do
      forM_ [0 .. lenXs - 1] $ \i -> do
        x <- MG.unsafeRead xs i
        when (x /= zero) $
          MG.unsafeModify
            ys
            (\y -> y `minus` x `times` yx)
            (i + lenYs - lenXs)
      ys' <- dropWhileEndM isZero ys
      gcdM xs ys'
    | lenYs <= lenXs -> do
      forM_ [0 .. lenYs - 1] $ \i -> do
        y <- MG.unsafeRead ys i
        MG.unsafeModify
          xs
          (\x -> x `times` zx `minus` y `times` zy)
          (i + lenXs - lenYs)
      forM_ [0 .. lenXs - lenYs - 1] $
        MG.unsafeModify xs (`times` zx)
      xs' <- dropWhileEndM isZero xs
      gcdM xs' ys
    | otherwise -> do
      forM_ [0 .. lenXs - 1] $ \i -> do
        x <- MG.unsafeRead xs i
        MG.unsafeModify
          ys
          (\y -> y `times` zy `minus` x `times` zx)
          (i + lenYs - lenXs)
      forM_ [0 .. lenYs - lenXs - 1] $
        MG.unsafeModify ys (`times` zy)
      ys' <- dropWhileEndM isZero ys
      gcdM xs ys'
{-# INLINABLE gcdM #-}

divide' :: GcdDomain a => a -> a -> a
divide' = (fromMaybe (error "gcd: violated internal invariant") .) . divide

isZeroM
  :: (Eq a, Semiring a, PrimMonad m, G.Vector v a)
  => G.Mutable v (PrimState m) a
  -> m Bool
isZeroM xs = go (MG.length xs)
  where
    go 0 = pure True
    go n = do
      x <- MG.unsafeRead xs (n - 1)
      if x == zero then go (n - 1) else pure False
{-# INLINE isZeroM #-}

quotient
  :: (Eq a, Ring a, GcdDomain a, G.Vector v a)
  => v a
  -> v a
  -> Maybe (v a)
quotient xs ys
  | G.null ys = throw DivideByZero
  | G.null xs = Just xs
  | G.length xs < G.length ys = Nothing
  | otherwise = runST $ do
    let lenXs = G.length xs
        lenYs = G.length ys
        lenQs = lenXs - lenYs + 1
    qs <- MG.unsafeNew lenQs
    rs <- MG.unsafeNew lenXs
    G.unsafeCopy rs xs

    let go i
          | i < 0 = do
            b <- isZeroM rs
            if b
              then Just <$> G.unsafeFreeze qs
              else pure Nothing
          | otherwise = do
            r <- MG.unsafeRead rs (lenYs - 1 + i)
            case r `divide` G.unsafeLast ys of
              Nothing -> pure Nothing
              Just q -> do
                MG.unsafeWrite qs i q
                forM_ [0 .. lenYs - 1] $ \k ->
                  MG.unsafeModify
                    rs
                    (\c -> c `minus` q `times` G.unsafeIndex ys k)
                    (i + k)
                go (i - 1)

    go (lenQs - 1)
{-# INLINABLE quotient #-}
