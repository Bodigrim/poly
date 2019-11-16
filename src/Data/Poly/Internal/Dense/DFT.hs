-- |
-- Module:      Data.Poly.Internal.Dense.FFT
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Discrete Fourier transform.
--

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Poly.Internal.Dense.DFT
  ( dft
  , inverseDft
  ) where

import Prelude hiding (recip, fromIntegral)
import Control.Monad.ST
import Data.Bits hiding (shift)
import Data.Foldable
import Data.Semiring (Semiring(..), Ring(..), minus, fromIntegral)
import Data.Field (Field, recip)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | <https://en.wikipedia.org/wiki/Fast_Fourier_transform Discrete Fourier transform>
-- \( y_k = \sum_{j=0}^{N-1} x_j \sqrt[N]{1}^{jk} \).
--
-- @since 0.5.0.0
dft
  :: (Ring a, G.Vector v a)
  => a   -- ^ primitive root \( \sqrt[N]{1} \), otherwise behaviour is undefined
  -> v a -- ^ \( \{ x_k \}_{k=0}^{N-1} \) (currently only  \( N = 2^n \) is supported)
  -> v a -- ^ \( \{ y_k \}_{k=0}^{N-1} \)
dft primRoot (xs :: v a)
  | popCount nn /= 1 = error "dft: only vectors of length 2^n are supported"
  | otherwise = go 0 0
  where
    nn = G.length xs
    n = countTrailingZeros nn

    roots :: v a
    roots = G.iterateN
      (1 `unsafeShiftL` (n - 1))
      (\x -> x `seq` (x `times` primRoot))
      one

    go !offset !shift
      | shift >= n = G.slice offset 1 xs
      | otherwise = runST $ do
        let halfLen = 1 `unsafeShiftL` (n - shift - 1)
            ys0 = go offset (shift + 1)
            ys1 = go (offset + 1 `unsafeShiftL` shift) (shift + 1)
        ys <- MG.new (halfLen `unsafeShiftL` 1)

        -- This corresponds to k = 0 in the loop below.
        -- It improves performance by avoiding multiplication
        -- by roots V.! 0 = 1.
        let y00 = (G.!) ys0 0
            y10 = (G.!) ys1 0
        MG.write ys 0       $! y00 `plus`  y10
        MG.write ys halfLen $! y00 `minus` y10

        forM_ [1..halfLen - 1] $ \k -> do
          let y0 = (G.!) ys0 k
              y1 = (G.!) ys1 k `times`
                   (G.!) roots (k `unsafeShiftL` shift)
          MG.write ys k             $! y0 `plus`  y1
          MG.write ys (k + halfLen) $! y0 `minus` y1
        G.unsafeFreeze ys
{-# INLINABLE dft #-}

-- | Inverse <https://en.wikipedia.org/wiki/Fast_Fourier_transform discrete Fourier transform>
-- \( x_k = {1\over N} \sum_{j=0}^{N-1} y_j \sqrt[N]{1}^{-jk} \).
--
-- @since 0.5.0.0
inverseDft
  :: (Field a, G.Vector v a)
  => a   -- ^ primitive root \( \sqrt[N]{1} \), otherwise behaviour is undefined
  -> v a -- ^ \( \{ y_k \}_{k=0}^{N-1} \) (currently only  \( N = 2^n \) is supported)
  -> v a -- ^ \( \{ x_k \}_{k=0}^{N-1} \)
inverseDft primRoot ys = G.map (`times` invN) $ dft (recip primRoot) ys
  where
    invN = recip $ fromIntegral $ G.length ys
{-# INLINABLE inverseDft #-}
