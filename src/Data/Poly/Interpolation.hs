module Data.Poly.Interpolation
  ( lagrange
  , hermite
  ) where

import Prelude hiding (Foldable(..))
import Data.Foldable

import Data.Poly.Internal.Dense
import qualified Data.Vector.Generic as G

-- | Compute the [Lagrange interpolating polynomial](https://en.wikipedia.org/wiki/Lagrange_polynomial).
--
-- This is the (unique) polynomial of minimal degree interpolating the given points.
-- The values are given as @(x, y)@ pairs where @y@ is the value at @x@. The @x@ values must be distinct.
lagrange :: (G.Vector v a, Eq a, Fractional a) => [(a, a)] -> Poly v a
lagrange = fst . foldl' f (0, 1)
  where
    f (p, w) (x, y) =
      let a = (y - eval p x) / eval w x
      in (p + scale 0 a w, scale 1 1 w - scale 0 x w) -- (p + a * w, w * (X - x))
{-# INLINABLE lagrange #-}

-- | Compute the [Hermite interpolating polynomial](https://en.wikipedia.org/wiki/Hermite_interpolation).
--
-- This is the (unique) polynomial of minimal degree interpolating the given points and derivatives.
-- The values are given as @(x, ys)@ pairs where @ys !! k@ is the k-th derivative at @x@. The @x@ values must be distinct.
hermite :: (G.Vector v a, Eq a, Fractional a) => [(a, [a])] -> Poly v a
hermite = fst . foldl' f (0, 1)
  where
    f (p, w) (x, ys) = let (_, p', w') = foldl' g (0, p, w) ys in (p', w')
      where
        g (k, p', w') y =
          let a = (y - evalk k p' x) / evalk k w' x
          in (k + 1, p' + scale 0 a w', scale 1 1 w' - scale 0 x w')
{-# INLINABLE hermite #-}
