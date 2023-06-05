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
  , Monom(..)
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Ord
import Data.Poly.Internal.Multi.Monom
import qualified Data.Vector.Algorithms.Tim as Tim
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.TypeNats

normalize
  :: G.Vector v (Monom n a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Monom n a)
  -> v (Monom n a)
normalize p add vs
  | G.null vs = vs
  | otherwise = runST $ do
    ws <- G.thaw vs
    l' <- normalizeM p add ws
    G.unsafeFreeze $ MG.unsafeSlice 0 l' ws
{-# INLINABLE normalize #-}

normalizeM
  :: G.Vector v (Monom n a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> G.Mutable v s (Monom n a)
  -> ST s Int
normalizeM p add ws = do
    let l = MG.length ws
    let go i j acc@(Monom accP accC)
          | j >= l =
            if p accC
              then do
                MG.write ws i acc
                pure $ i + 1
              else pure i
          | otherwise = do
            v@(Monom vp vc) <- MG.unsafeRead ws j
            if vp == accP
              then go i (j + 1) (Monom accP (accC `add` vc))
              else if p accC
                then do
                  MG.write ws i acc
                  go (i + 1) (j + 1) v
                else go i (j + 1) v
    Tim.sortBy (comparing (\(Monom w _) -> w)) ws
    wsHead <- MG.unsafeRead ws 0
    go 0 1 wsHead
{-# INLINABLE normalizeM #-}

plusPoly
  :: G.Vector v (Monom n a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Monom n a)
  -> v (Monom n a)
  -> v (Monom n a)
plusPoly p add = \xs ys -> runST $ do
  zs <- MG.unsafeNew (G.length xs + G.length ys)
  lenZs <- plusPolyM p add xs ys zs
  G.unsafeFreeze $ MG.unsafeSlice 0 lenZs zs
{-# INLINABLE plusPoly #-}

plusPolyM
  :: G.Vector v (Monom n a)
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Monom n a)
  -> v (Monom n a)
  -> G.Mutable v s (Monom n a)
  -> ST s Int
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
      | (Monom xp xc) <- G.unsafeIndex xs ix
      , (Monom yp yc) <- G.unsafeIndex ys iy
      = case xp `compare` yp of
        LT -> do
          MG.unsafeWrite zs iz (Monom xp xc)
          go (ix + 1) iy (iz + 1)
        EQ -> do
          let zc = xc `add` yc
          if p zc then do
            MG.unsafeWrite zs iz (Monom xp zc)
            go (ix + 1) (iy + 1) (iz + 1)
          else
            go (ix + 1) (iy + 1) iz
        GT -> do
          MG.unsafeWrite zs iz (Monom yp yc)
          go ix (iy + 1) (iz + 1)
{-# INLINE plusPolyM #-}

minusPoly
  :: G.Vector v (Monom n a)
  => (a -> Bool)
  -> (a -> a)
  -> (a -> a -> a)
  -> v (Monom n a)
  -> v (Monom n a)
  -> v (Monom n a)
minusPoly p neg sub = \xs ys -> runST $ do
  let lenXs = G.length xs
      lenYs = G.length ys
  zs <- MG.unsafeNew (lenXs + lenYs)
  let go ix iy iz
        | ix == lenXs, iy == lenYs = pure iz
        | ix == lenXs = do
          forM_ [iy .. lenYs - 1] $ \i ->
            MG.unsafeWrite zs (iz + i - iy)
              ((\(Monom ps c) -> Monom ps (neg c)) (G.unsafeIndex ys i))
          pure $ iz + lenYs - iy
        | iy == lenYs = do
          G.unsafeCopy
            (MG.unsafeSlice iz (lenXs - ix) zs)
            (G.unsafeSlice ix (lenXs - ix) xs)
          pure $ iz + lenXs - ix
        | (Monom xp xc) <- G.unsafeIndex xs ix
        , (Monom yp yc) <- G.unsafeIndex ys iy
        = case xp `compare` yp of
          LT -> do
            MG.unsafeWrite zs iz (Monom xp xc)
            go (ix + 1) iy (iz + 1)
          EQ -> do
            let zc = xc `sub` yc
            if p zc then do
              MG.unsafeWrite zs iz (Monom xp zc)
              go (ix + 1) (iy + 1) (iz + 1)
            else
              go (ix + 1) (iy + 1) iz
          GT -> do
            MG.unsafeWrite zs iz (Monom yp (neg yc))
            go ix (iy + 1) (iz + 1)
  lenZs <- go 0 0 0
  G.unsafeFreeze $ MG.unsafeSlice 0 lenZs zs
{-# INLINABLE minusPoly #-}

scaleM
  :: (KnownNat n, G.Vector v (Monom n a))
  => (a -> Bool)
  -> (a -> a -> a)
  -> v (Monom n a)
  -> (Monom n a)
  -> G.Mutable v s (Monom n a)
  -> ST s Int
scaleM p mul xs (Monom yp yc) zs = go 0 0
  where
    lenXs = G.length xs

    go ix iz
      | ix == lenXs = pure iz
      | (Monom xp xc) <- G.unsafeIndex xs ix
      = do
        let zc = xc `mul` yc
        if p zc then do
          MG.unsafeWrite zs iz (Monom (xp + yp) zc)
          go (ix + 1) (iz + 1)
        else
          go (ix + 1) iz
{-# INLINABLE scaleM #-}

scaleInternal
  :: (KnownNat n, G.Vector v (Monom n a))
  => (a -> Bool)
  -> (a -> a -> a)
  -> SU.Vector n Word
  -> a
  -> v (Monom n a)
  -> v (Monom n a)
scaleInternal p mul yp yc xs = runST $ do
  zs <- MG.unsafeNew (G.length xs)
  len <- scaleM p (flip mul) xs (Monom yp yc) zs
  G.unsafeFreeze $ MG.unsafeSlice 0 len zs
{-# INLINABLE scaleInternal #-}

convolution
  :: forall v n a.
     (KnownNat n, G.Vector v (Monom n a))
  => (a -> Bool)
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v (Monom n a)
  -> v (Monom n a)
  -> v (Monom n a)
convolution p add mult = \xs ys ->
  if G.length xs >= G.length ys
  then go mult xs ys
  else go (flip mult) ys xs
  where
    go :: (a -> a -> a) -> v (Monom n a) -> v (Monom n a) -> v (Monom n a)
    go mul long short = runST $ do
      let lenLong   = G.length long
          lenShort  = G.length short
          lenBuffer = lenLong * lenShort
      slices <- MG.unsafeNew lenShort
      buffer <- MG.unsafeNew lenBuffer

      forM_ [0 .. lenShort - 1] $ \iShort -> do
        let (Monom pShort cShort) = G.unsafeIndex short iShort
            from = iShort * lenLong
            bufferSlice = MG.unsafeSlice from lenLong buffer
        len <- scaleM p mul long (Monom pShort cShort) bufferSlice
        MG.unsafeWrite slices iShort (from, len)

      slices' <- G.unsafeFreeze slices
      buffer' <- G.unsafeFreeze buffer
      bufferNew <- MG.unsafeNew lenBuffer
      gogo slices' buffer' bufferNew

    gogo
      :: U.Vector (Int, Int)
      -> v (Monom n a)
      -> G.Mutable v s (Monom n a)
      -> ST s (v (Monom n a))
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
  :: G.Vector v (Monom n a)
  => (a -> Bool)
  -- ^ is coefficient non-zero?
  -> (SU.Vector n Word -> SU.Vector n Word)
  -- ^ how to modify powers?
  -> (SU.Vector n Word -> a -> a)
  -- ^ how to modify coefficient?
  -> v (Monom n a)
  -> v (Monom n a)
derivPoly p dec mul xs
  | G.null xs = G.empty
  | otherwise = runST $ do
    let lenXs = G.length xs
    zs <- MG.unsafeNew lenXs
    let go ix iz
          | ix == lenXs = pure iz
          | (Monom xp xc) <- G.unsafeIndex xs ix
          = do
            let zc = xp `mul` xc
            if p zc then do
              MG.unsafeWrite zs iz (Monom (dec xp) zc)
              go (ix + 1) (iz + 1)
            else
              go (ix + 1) iz
    lenZs <- go 0 0
    G.unsafeFreeze $ MG.unsafeSlice 0 lenZs zs
{-# INLINABLE derivPoly #-}
