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
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

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
    G.unsafeFreeze $ MG.unsafeSlice 0 l' ws

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
            v@(vp, vc) <- MG.unsafeRead ws j
            if vp == accP
              then go i (j + 1) (accP, accC `add` vc)
              else if p accC
                then do
                  MG.write ws i acc
                  go (i + 1) (j + 1) v
                else go i (j + 1) v
    Tim.sortBy (comparing fst) ws
    wsHead <- MG.unsafeRead ws 0
    go 0 1 wsHead

plusPoly
  :: (G.Vector v (t, a), Ord t)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (t, a)
  -> v (t, a)
  -> v (t, a)
plusPoly p add xs ys = runST $ do
  zs <- MG.unsafeNew (G.length xs + G.length ys)
  lenZs <- plusPolyM p add xs ys zs
  G.unsafeFreeze $ MG.unsafeSlice 0 lenZs zs
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
        G.unsafeCopy
          (MG.unsafeSlice iz (lenYs - iy) zs)
          (G.unsafeSlice iy (lenYs - iy) ys)
        pure $ iz + lenYs - iy
      | iy == lenYs = do
        G.unsafeCopy
          (MG.unsafeSlice iz (lenXs - ix) zs)
          (G.unsafeSlice ix (lenXs - ix) xs)
        pure $ iz + lenXs - ix
      | (xp, xc) <- G.unsafeIndex xs ix
      , (yp, yc) <- G.unsafeIndex ys iy
      = case xp `compare` yp of
        LT -> do
          MG.unsafeWrite zs iz (xp, xc)
          go (ix + 1) iy (iz + 1)
        EQ -> do
          let zc = xc `add` yc
          if p zc then do
            MG.unsafeWrite zs iz (xp, zc)
            go (ix + 1) (iy + 1) (iz + 1)
          else
            go (ix + 1) (iy + 1) iz
        GT -> do
          MG.unsafeWrite zs iz (yp, yc)
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
  zs <- MG.unsafeNew (lenXs + lenYs)
  let go ix iy iz
        | ix == lenXs, iy == lenYs = pure iz
        | ix == lenXs = do
          forM_ [iy .. lenYs - 1] $ \i ->
            MG.unsafeWrite zs (iz + i - iy)
              (fmap neg (G.unsafeIndex ys i))
          pure $ iz + lenYs - iy
        | iy == lenYs = do
          G.unsafeCopy
            (MG.unsafeSlice iz (lenXs - ix) zs)
            (G.unsafeSlice ix (lenXs - ix) xs)
          pure $ iz + lenXs - ix
        | (xp, xc) <- G.unsafeIndex xs ix
        , (yp, yc) <- G.unsafeIndex ys iy
        = case xp `compare` yp of
          LT -> do
            MG.unsafeWrite zs iz (xp, xc)
            go (ix + 1) iy (iz + 1)
          EQ -> do
            let zc = xc `sub` yc
            if p zc then do
              MG.unsafeWrite zs iz (xp, zc)
              go (ix + 1) (iy + 1) (iz + 1)
            else
              go (ix + 1) (iy + 1) iz
          GT -> do
            MG.unsafeWrite zs iz (yp, neg yc)
            go ix (iy + 1) (iz + 1)
  lenZs <- go 0 0 0
  G.unsafeFreeze $ MG.unsafeSlice 0 lenZs zs
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
      | (xp, xc) <- G.unsafeIndex xs ix
      = do
        let zc = xc `mul` yc
        if p zc then do
          MG.unsafeWrite zs iz (xp + yp, zc)
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
  zs <- MG.unsafeNew (G.length xs)
  len <- scaleM p (flip mul) xs (yp, yc) zs
  G.unsafeFreeze $ MG.unsafeSlice 0 len zs
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
      slices <- MG.unsafeNew lenShort
      buffer <- MG.unsafeNew lenBuffer

      forM_ [0 .. lenShort - 1] $ \iShort -> do
        let (pShort, cShort) = G.unsafeIndex short iShort
            from = iShort * lenLong
            bufferSlice = MG.unsafeSlice from lenLong buffer
        len <- scaleM p mul long (pShort, cShort) bufferSlice
        MG.unsafeWrite slices iShort (from, len)

      slices' <- G.unsafeFreeze slices
      buffer' <- G.unsafeFreeze buffer
      bufferNew <- MG.unsafeNew lenBuffer
      gogo slices' buffer' bufferNew

    gogo
      :: PrimMonad m
      => U.Vector (Int, Int)
      -> v (t, a)
      -> G.Mutable v (PrimState m) (t, a)
      -> m (v (t, a))
    gogo slices buffer bufferNew
      | G.length slices == 0
      = pure G.empty
      | G.length slices == 1
      , (from, len) <- G.unsafeIndex slices 0
      = pure $ G.unsafeSlice from len buffer
      | otherwise = do
        let nSlices = G.length slices
        slicesNew <- MG.unsafeNew ((nSlices + 1) `shiftR` 1)
        forM_ [0 .. (nSlices - 2) `shiftR` 1] $ \i -> do
          let (from1, len1) = G.unsafeIndex slices (2 * i)
              (from2, len2) = G.unsafeIndex slices (2 * i + 1)
              slice1 = G.unsafeSlice from1 len1 buffer
              slice2 = G.unsafeSlice from2 len2 buffer
              slice3 = MG.unsafeSlice from1 (len1 + len2) bufferNew
          len3 <- plusPolyM p add slice1 slice2 slice3
          MG.unsafeWrite slicesNew i (from1, len3)

        when (odd nSlices) $ do
          let (from, len) = G.unsafeIndex slices (nSlices - 1)
              slice1 = G.unsafeSlice from len buffer
              slice3 = MG.unsafeSlice from len bufferNew
          G.unsafeCopy slice3 slice1
          MG.unsafeWrite slicesNew (nSlices `shiftR` 1) (from, len)

        slicesNew' <- G.unsafeFreeze slicesNew
        buffer'    <- G.unsafeThaw   buffer
        bufferNew' <- G.unsafeFreeze bufferNew
        gogo slicesNew' bufferNew' buffer'
{-# INLINABLE convolution #-}

derivPoly
  :: (G.Vector v (t, a))
  => (a -> Bool)
  -> (t -> t)
  -> (t -> a -> a)
  -> v (t, a)
  -> v (t, a)
derivPoly p dec mul xs
  | G.null xs = G.empty
  | otherwise = runST $ do
    let lenXs = G.length xs
    zs <- MG.unsafeNew lenXs
    let go ix iz
          | ix == lenXs = pure iz
          | (xp, xc) <- G.unsafeIndex xs ix
          = do
            let zc = xp `mul` xc
            if p zc then do
              MG.unsafeWrite zs iz (dec xp, zc)
              go (ix + 1) (iz + 1)
            else
              go (ix + 1) iz
    lenZs <- go 0 0
    G.unsafeFreeze $ MG.unsafeSlice 0 lenZs zs
{-# INLINABLE derivPoly #-}
