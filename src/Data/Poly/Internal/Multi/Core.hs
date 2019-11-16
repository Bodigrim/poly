-- |
-- Module:      Data.Poly.Internal.Multi.Core
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials of one variable.
--

{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Data.Poly.Internal.Multi.Core
  ( normalize
  , plusPoly
  , minusPoly
  , convolution
  , scaleInternal
  , derivPoly
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Ord
import qualified Data.Vector.Algorithms.Tim as Tim
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U

normalize
  :: (G.Vector v (t, a), Ord t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (t, a)
  -> v (t, a)
normalize p add vs
  | G.null vs = vs
  | otherwise = runST $ do
    ws <- G.thaw vs
    l' <- normalizeM p add ws
    G.unsafeFreeze $ MG.slice 0 l' ws
{-# INLINABLE normalize #-}

normalizeM
  :: (PrimMonad m, G.Vector v (t, a), Ord t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> G.Mutable v (PrimState m) (t, a)
  -> m Int
normalizeM p add ws = do
    let l = MG.length ws
    let go i j acc@(accP, accC)
          | j >= l =
            if p accC
              then do
                MG.write ws i acc
                pure $ i + 1
              else pure i
          | otherwise = do
            v@(vp, vc) <- MG.read ws j
            if vp == accP
              then go i (j + 1) (accP, accC `add` vc)
              else if p accC
                then do
                  MG.write ws i acc
                  go (i + 1) (j + 1) v
                else go i (j + 1) v
    Tim.sortBy (comparing fst) ws
    wsHead <- MG.read ws 0
    go 0 1 wsHead
{-# INLINABLE normalizeM #-}

plusPoly
  :: (G.Vector v (t, a), Ord t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (t, a)
  -> v (t, a)
  -> v (t, a)
plusPoly p add xs ys = runST $ do
  zs <- MG.new (G.length xs + G.length ys)
  lenZs <- plusPolyM p add xs ys zs
  G.unsafeFreeze $ MG.slice 0 lenZs zs
{-# INLINABLE plusPoly #-}

plusPolyM
  :: (PrimMonad m, G.Vector v (t, a), Ord t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (t, a)
  -> v (t, a)
  -> G.Mutable v (PrimState m) (t, a)
  -> m Int
plusPolyM p add xs ys zs = go 0 0 0
  where
    lenXs = G.length xs
    lenYs = G.length ys

    go ix iy iz
      | ix == lenXs, iy == lenYs = pure iz
      | ix == lenXs = do
        G.copy
          (MG.slice iz (lenYs - iy) zs)
          (G.slice iy (lenYs - iy) ys)
        pure $ iz + lenYs - iy
      | iy == lenYs = do
        G.copy
          (MG.slice iz (lenXs - ix) zs)
          (G.slice ix (lenXs - ix) xs)
        pure $ iz + lenXs - ix
      | (xp, xc) <- (G.!) xs ix
      , (yp, yc) <- (G.!) ys iy
      = case xp `compare` yp of
        LT -> do
          MG.write zs iz (xp, xc)
          go (ix + 1) iy (iz + 1)
        EQ -> do
          let zc = xc `add` yc
          if p zc then do
            MG.write zs iz (xp, zc)
            go (ix + 1) (iy + 1) (iz + 1)
          else
            go (ix + 1) (iy + 1) iz
        GT -> do
          MG.write zs iz (yp, yc)
          go ix (iy + 1) (iz + 1)
{-# INLINABLE plusPolyM #-}

minusPoly
  :: (G.Vector v (t, a), Ord t)
  => (a -> Bool)
  -> (a -> a)
  -> (a -> a -> a)
  -> v (t, a)
  -> v (t, a)
  -> v (t, a)
minusPoly p neg sub xs ys = runST $ do
  zs <- MG.new (lenXs + lenYs)
  let go ix iy iz
        | ix == lenXs, iy == lenYs = pure iz
        | ix == lenXs = do
          forM_ [iy .. lenYs - 1] $ \i ->
            MG.write zs (iz + i - iy)
              (fmap neg ((G.!) ys i))
          pure $ iz + lenYs - iy
        | iy == lenYs = do
          G.copy
            (MG.slice iz (lenXs - ix) zs)
            (G.slice ix (lenXs - ix) xs)
          pure $ iz + lenXs - ix
        | (xp, xc) <- (G.!) xs ix
        , (yp, yc) <- (G.!) ys iy
        = case xp `compare` yp of
          LT -> do
            MG.write zs iz (xp, xc)
            go (ix + 1) iy (iz + 1)
          EQ -> do
            let zc = xc `sub` yc
            if p zc then do
              MG.write zs iz (xp, zc)
              go (ix + 1) (iy + 1) (iz + 1)
            else
              go (ix + 1) (iy + 1) iz
          GT -> do
            MG.write zs iz (yp, neg yc)
            go ix (iy + 1) (iz + 1)
  lenZs <- go 0 0 0
  G.unsafeFreeze $ MG.slice 0 lenZs zs
  where
    lenXs = G.length xs
    lenYs = G.length ys
{-# INLINABLE minusPoly #-}

scaleM
  :: (PrimMonad m, G.Vector v (t, a), Num t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (t, a)
  -> (t, a)
  -> G.Mutable v (PrimState m) (t, a)
  -> m Int
scaleM p mul xs (yp, yc) zs = go 0 0
  where
    lenXs = G.length xs

    go ix iz
      | ix == lenXs = pure iz
      | (xp, xc) <- (G.!) xs ix
      = do
        let zc = xc `mul` yc
        if p zc then do
          MG.write zs iz (xp + yp, zc)
          go (ix + 1) (iz + 1)
        else
          go (ix + 1) iz
{-# INLINABLE scaleM #-}

scaleInternal
  :: (G.Vector v (t, a), Num t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> t
  -> a
  -> v (t, a)
  -> v (t, a)
scaleInternal p mul yp yc xs = runST $ do
  zs <- MG.new (G.length xs)
  len <- scaleM p (flip mul) xs (yp, yc) zs
  G.unsafeFreeze $ MG.slice 0 len zs
{-# INLINABLE scaleInternal #-}

convolution
  :: forall v t a.
     (G.Vector v (t, a), Ord t, Num t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v (t, a)
  -> v (t, a)
  -> v (t, a)
convolution p add mult xs ys
  | G.length xs >= G.length ys
  = go mult xs ys
  | otherwise
  = go (flip mult) ys xs
  where
    go :: (a -> a -> a) -> v (t, a) -> v (t, a) -> v (t, a)
    go mul long short = runST $ do
      let lenLong   = G.length long
          lenShort  = G.length short
          lenBuffer = lenLong * lenShort
      slices <- MG.new lenShort
      buffer <- MG.new lenBuffer

      forM_ [0 .. lenShort - 1] $ \iShort -> do
        let (pShort, cShort) = (G.!) short iShort
            from = iShort * lenLong
            bufferSlice = MG.slice from lenLong buffer
        len <- scaleM p mul long (pShort, cShort) bufferSlice
        MG.write slices iShort (from, len)

      slices' <- G.unsafeFreeze slices
      buffer' <- G.unsafeFreeze buffer
      bufferNew <- MG.new lenBuffer
      gogo slices' buffer' bufferNew

    gogo
      :: U.Vector (Int, Int)
      -> v (t, a)
      -> G.Mutable v s (t, a)
      -> ST s (v (t, a))
    gogo slices buffer bufferNew
      | G.length slices == 0
      = pure G.empty
      | G.length slices == 1
      , (from, len) <- (G.!) slices 0
      = pure $ G.slice from len buffer
      | otherwise = do
        let nSlices = G.length slices
        slicesNew <- MG.new ((nSlices + 1) `shiftR` 1)
        forM_ [0 .. (nSlices - 2) `shiftR` 1] $ \i -> do
          let (from1, len1) = (G.!) slices (2 * i)
              (from2, len2) = (G.!) slices (2 * i + 1)
              slice1 = G.slice from1 len1 buffer
              slice2 = G.slice from2 len2 buffer
              slice3 = MG.slice from1 (len1 + len2) bufferNew
          len3 <- plusPolyM p add slice1 slice2 slice3
          MG.write slicesNew i (from1, len3)

        when (odd nSlices) $ do
          let (from, len) = (G.!) slices (nSlices - 1)
              slice1 = G.slice from len buffer
              slice3 = MG.slice from len bufferNew
          G.copy slice3 slice1
          MG.write slicesNew (nSlices `shiftR` 1) (from, len)

        slicesNew' <- G.unsafeFreeze slicesNew
        buffer'    <- G.unsafeThaw   buffer
        bufferNew' <- G.unsafeFreeze bufferNew
        gogo slicesNew' bufferNew' buffer'
{-# INLINABLE convolution #-}

derivPoly
  :: (G.Vector v (t, a))
  => (a -> Bool)   -- ^ is coefficient non-zero?
  -> (t -> t)      -- ^ how to modify powers?
  -> (t -> a -> a) -- ^ how to modify coefficient?
  -> v (t, a)
  -> v (t, a)
derivPoly p dec mul xs
  | G.null xs = G.empty
  | otherwise = runST $ do
    let lenXs = G.length xs
    zs <- MG.new lenXs
    let go ix iz
          | ix == lenXs = pure iz
          | (xp, xc) <- (G.!) xs ix
          = do
            let zc = xp `mul` xc
            if p zc then do
              MG.write zs iz (dec xp, zc)
              go (ix + 1) (iz + 1)
            else
              go (ix + 1) iz
    lenZs <- go 0 0
    G.unsafeFreeze $ MG.slice 0 lenZs zs
{-# INLINABLE derivPoly #-}
