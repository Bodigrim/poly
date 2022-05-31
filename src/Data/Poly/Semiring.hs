-- |
-- Module:      Data.Poly.Semiring
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Semiring'-based interface.
--

{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

module Data.Poly.Semiring
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , leading
  , toPoly
  , monomial
  , scale
  , pattern X
  , eval
  , subst
  , deriv
  , integral
#ifdef SupportSparse
  , denseToSparse
  , sparseToDense
#endif
  , dft
  , inverseDft
  , dftMult
  ) where

import Data.Bits
import Data.Euclidean (Field)
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G

import Data.Poly.Internal.Dense (Poly(..), VPoly, UPoly, leading)
import qualified Data.Poly.Internal.Dense as Dense
import Data.Poly.Internal.Dense.Field ()
import Data.Poly.Internal.Dense.DFT
import Data.Poly.Internal.Dense.GcdDomain ()

#ifdef SupportSparse
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Poly.Internal.Multi as Sparse
import qualified Data.Poly.Internal.Convert as Convert
#endif

-- | Make a 'Poly' from a vector of coefficients
-- (first element corresponds to the constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [0,0,0] :: UPoly Int
-- 0
toPoly :: (Eq a, Semiring a, G.Vector v a) => v a -> Poly v a
toPoly = Dense.toPoly'

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a
monomial = Dense.monomial'

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 0 * X^3 + 3 * X^2 + 0 * X + 0
scale :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
scale = Dense.scale'

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, G.Vector v a) => Poly v a
pattern X = Dense.X'

-- | Evaluate the polynomial at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
eval :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval = Dense.eval'

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
subst :: (Eq a, Semiring a, G.Vector v a, G.Vector w a) => Poly v a -> Poly w a -> Poly w a
subst = Dense.subst'

-- | Take the derivative of the polynomial.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 0 * X + 3
deriv :: (Eq a, Semiring a, G.Vector v a) => Poly v a -> Poly v a
deriv = Dense.deriv'

-- | Compute an indefinite integral of the polynomial,
-- setting the constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
integral :: (Eq a, Field a, G.Vector v a) => Poly v a -> Poly v a
integral = Dense.integral'

-- | Multiplication of polynomials using
-- <https://en.wikipedia.org/wiki/Fast_Fourier_transform discrete Fourier transform>.
-- It could be faster than '(*)' for large polynomials
-- if multiplication of coefficients is particularly expensive.
dftMult
  :: (Eq a, Field a, G.Vector v a)
  => (Int -> a) -- ^ mapping from \( N = 2^n \) to a primitive root \( \sqrt[N]{1} \)
  -> Poly v a
  -> Poly v a
  -> Poly v a
dftMult getPrimRoot (Poly xs) (Poly ys) =
  toPoly $ inverseDft primRoot $ G.zipWith times (dft primRoot xs') (dft primRoot ys')
  where
    nextPowerOf2 :: Int -> Int
    nextPowerOf2 0 = 1
    nextPowerOf2 1 = 1
    nextPowerOf2 x = 1 `unsafeShiftL` (finiteBitSize (0 :: Int) - countLeadingZeros (x - 1))

    padTo l vs = G.generate l (\k -> if k < G.length vs then vs G.! k else zero)

    zl = nextPowerOf2 (G.length xs + G.length ys)
    xs' = padTo zl xs
    ys' = padTo zl ys
    primRoot = getPrimRoot zl
{-# INLINABLE dftMult #-}

#ifdef SupportSparse
-- | Convert from dense to sparse polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> denseToSparse (1 `plus` Data.Poly.X^2) :: Data.Poly.Sparse.UPoly Int
-- 1 * X^2 + 1
denseToSparse :: (Eq a, Semiring a, G.Vector v a, G.Vector v (SU.Vector 1 Word, a)) => Dense.Poly v a -> Sparse.Poly v a
denseToSparse = Convert.denseToSparse'

-- | Convert from sparse to dense polynomials.
--
-- >>> :set -XFlexibleContexts
-- >>> sparseToDense (1 `plus` Data.Poly.Sparse.X^2) :: Data.Poly.UPoly Int
-- 1 * X^2 + 0 * X + 1
sparseToDense :: (Semiring a, G.Vector v a, G.Vector v (SU.Vector 1 Word, a)) => Sparse.Poly v a -> Dense.Poly v a
sparseToDense = Convert.sparseToDense'
#endif
