-- |
-- Module:      Data.Poly.Internal.Convert
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Conversions between polynomials.
--

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Poly.Internal.Convert
  ( denseToSparse
  , denseToSparse'
  , sparseToDense
  , sparseToDense'
  ) where

import Control.Monad.ST
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed.Sized as SU

import qualified Data.Poly.Internal.Dense as Dense
import qualified Data.Poly.Internal.Multi as Sparse

-- | Convert from dense to sparse polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> denseToSparse (1 + Data.Poly.X^2) :: Data.Poly.Sparse.UPoly Int
-- 1 * X^2 + 1
--
-- @since 0.5.0.0
denseToSparse
  :: (Eq a, Num a, G.Vector v a, G.Vector v (Sparse.Monom 1 a))
  => Dense.Poly v a
  -> Sparse.Poly v a
denseToSparse = denseToSparseInternal 0

denseToSparse'
  :: (Eq a, Semiring a, G.Vector v a, G.Vector v (Sparse.Monom 1 a))
  => Dense.Poly v a
  -> Sparse.Poly v a
denseToSparse' = denseToSparseInternal zero

denseToSparseInternal
  :: (Eq a, G.Vector v a, G.Vector v (Sparse.Monom 1 a))
  => a
  -> Dense.Poly v a
  -> Sparse.Poly v a
denseToSparseInternal z = Sparse.MultiPoly . G.imapMaybe (\i c -> if c == z then Nothing else Just (Sparse.Monom (fromIntegral i) c)) . Dense.unPoly

-- | Convert from sparse to dense polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> sparseToDense (1 + Data.Poly.Sparse.X^2) :: Data.Poly.UPoly Int
-- 1 * X^2 + 0 * X + 1
--
-- @since 0.5.0.0
sparseToDense
  :: (Num a, G.Vector v a, G.Vector v (Sparse.Monom 1 a))
  => Sparse.Poly v a
  -> Dense.Poly v a
sparseToDense = sparseToDenseInternal 0

sparseToDense'
  :: (Semiring a, G.Vector v a, G.Vector v (Sparse.Monom 1 a))
  => Sparse.Poly v a
  -> Dense.Poly v a
sparseToDense' = sparseToDenseInternal zero

sparseToDenseInternal
  :: (G.Vector v a, G.Vector v (Sparse.Monom 1 a))
  => a
  -> Sparse.Poly v a
  -> Dense.Poly v a
sparseToDenseInternal z (Sparse.MultiPoly xs)
  | G.null xs = Dense.Poly G.empty
  | otherwise = runST $ do
  let len = fromIntegral (SU.head (Sparse.monomPower (G.unsafeLast xs)) + 1)
  ys <- MG.unsafeNew len
  MG.set ys z
  let go xi yi
        | xi >= G.length xs = pure ()
        | Sparse.Monom yi' c <- G.unsafeIndex xs xi
        , yi == fromIntegral (SU.head yi')
        = MG.unsafeWrite ys yi c >> go (xi + 1) (yi + 1)
        | otherwise = go xi (yi + 1)
  go 0 0
  Dense.Poly <$> G.unsafeFreeze ys
