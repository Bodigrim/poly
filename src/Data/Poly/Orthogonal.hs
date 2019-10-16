-- |
-- Module:      Data.Poly.Orthogonal
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Classical orthogonal polynomials.
--

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE RebindableSyntax    #-}

#if !MIN_VERSION_semirings(0,5,0)

module Data.Poly.Orthogonal () where

#else

module Data.Poly.Orthogonal
  ( legendre
  , legendreShifted
  , gegenbauer
  , jacobi
  , chebyshev1
  , chebyshev2
  , hermiteProb
  , hermitePhys
  , laguerre
  , laguerreGen
  ) where

import Prelude hiding (quot, Num(..), fromIntegral)
import Data.Euclidean
import Data.Semiring
import Data.Poly.Semiring
import Data.Poly.Internal.Dense (unscale')
import Data.Vector.Generic (Vector, fromListN)

-- | <https://en.wikipedia.org/wiki/Legendre_polynomials Legendre polynomials>.
--
-- >>> take 3 legendre :: [Data.Poly.VPoly Double]
-- [1.0,1.0 * X + 0.0,1.5 * X^2 + 0.0 * X + (-0.5)]
legendre :: (Eq a, Field a, Vector v a) => [Poly v a]
legendre = map (flip subst' (toPoly [1 `quot` 2, 1 `quot` 2])) legendreShifted
  where
    subst' :: (Eq a, Semiring a, Vector v a) => Poly v a -> Poly v a -> Poly v a
    subst' = subst

-- | <https://en.wikipedia.org/wiki/Legendre_polynomials#Shifted_Legendre_polynomials Shifted Legendre polynomials>.
--
-- >>> take 3 legendreShifted :: [Data.Poly.VPoly Integer]
-- [1,2 * X + (-1),6 * X^2 + (-6) * X + 1]
legendreShifted :: (Eq a, Euclidean a, Ring a, Vector v a) => [Poly v a]
legendreShifted = xs
  where
    xs = 1 : toPoly [-1, 2] : zipWith3 rec (iterate (+ 1) 1) xs (tail xs)
    rec n pm1 p = unscale' 0 (n + 1) (toPoly [-1 - 2 * n, 2 + 4 * n] * p - scale 0 n pm1)

-- | <https://en.wikipedia.org/wiki/Gegenbauer_polynomials Gegenbauer polynomials>.
gegenbauer :: (Eq a, Field a, Vector v a) => a -> [Poly v a]
gegenbauer g = jacobi a a
  where
    a = g - 1 `quot` 2

-- | <https://en.wikipedia.org/wiki/Jacobi_polynomials Jacobi polynomials>.
jacobi :: (Eq a, Field a, Vector v a) => a -> a -> [Poly v a]
jacobi a b = xs
  where
    x0 = 1
    x1 = toPoly [(a - b) `quot` 2, (a + b + 2) `quot` 2]
    xs = x0 : x1 : zipWith3 rec (iterate (+ 1) 2) xs (tail xs)
    rec n pm1 p = toPoly [d, c] * p - scale 0 cm1 pm1
      where
        cp1 = 2 * n * (n + a + b) * (2 * n + a + b - 2)
        q   = (2 * n + a + b - 1) `quot` cp1
        c   = q * ((2 * n + a + b) * (2 * n + a + b - 2))
        d   = q * (a * a - b * b)
        cm1 = 2 * (n + a - 1) * (n + b - 1) * (2 * n + a + b) `quot` cp1

-- | <https://en.wikipedia.org/wiki/Chebyshev_polynomials Chebyshev polynomials>
-- of the first kind.
--
-- >>> take 3 chebyshev1 :: [VPoly Integer]
-- [1,1 * X + 0,2 * X^2 + 0 * X + (-1)]
chebyshev1 :: (Eq a, Ring a, Vector v a) => [Poly v a]
chebyshev1 = xs
  where
    xs = 1 : monomial 1 1 : zipWith (\pm1 p -> scale 1 2 p - pm1) xs (tail xs)

-- | <https://en.wikipedia.org/wiki/Chebyshev_polynomials Chebyshev polynomials>
-- of the second kind.
--
-- >>> take 3 chebyshev2 :: [VPoly Integer]
-- [1,2 * X + 0,4 * X^2 + 0 * X + (-1)]
chebyshev2 :: (Eq a, Ring a, Vector v a) => [Poly v a]
chebyshev2 = xs
  where
    xs = 1 : monomial 1 2 : zipWith (\pm1 p -> scale 1 2 p - pm1) xs (tail xs)

-- | Probabilists' <https://en.wikipedia.org/wiki/Hermite_polynomials Hermite polynomials>.
--
-- >>> take 3 hermiteProb :: [VPoly Integer]
-- [1,1 * X + 0,1 * X^2 + 0 * X + (-1)]
hermiteProb :: (Eq a, Ring a, Vector v a) => [Poly v a]
hermiteProb = xs
  where
    xs = 1 : monomial 1 1 : zipWith3 rec (iterate (+ 1) 1) xs (tail xs)
    rec n pm1 p = scale 1 1 p - scale 0 n pm1

-- | Physicists' <https://en.wikipedia.org/wiki/Hermite_polynomials Hermite polynomials>.
--
-- >>> take 3 hermitePhys :: [VPoly Double]
-- [1,2 * X + 0,4 * X^2 + 0 * X + (-2)]
hermitePhys :: (Eq a, Ring a, Vector v a) => [Poly v a]
hermitePhys = xs
  where
    xs = 1 : monomial 1 2 : zipWith3 rec (iterate (+ 1) 1) xs (tail xs)
    rec n pm1 p = scale 1 2 p - scale 0 (2 * n) pm1

-- | <https://en.wikipedia.org/wiki/Laguerre_polynomials Laguerre polynomials>.
--
-- >>> take 3 laguerre :: [VPoly Double]
-- [1.0,(-1.0) * X + 1.0,0.5 * X^2 + (-2.0) * X + 1.0]
laguerre :: (Eq a, Field a, Vector v a) => [Poly v a]
laguerre = laguerreGen 0

-- | <https://en.wikipedia.org/wiki/Laguerre_polynomials#Generalized_Laguerre_polynomials Generalized Laguerre polynomials>
laguerreGen :: (Eq a, Field a, Vector v a) => a -> [Poly v a]
laguerreGen a = xs
  where
    x0 = 1
    x1 = toPoly [1 + a, -1]
    xs = x0 : x1 : zipWith3 rec (iterate (+ 1) 1) xs (tail xs)
    rec n pm1 p = toPoly [(2 * n + 1 + a) `quot` (n + 1), -1 `quot` (n + 1)] * p - scale 0 ((n + a) `quot` (n + 1)) pm1

#endif
