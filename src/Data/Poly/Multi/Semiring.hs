-- |
-- Module:      Data.Poly.Multi.Semiring
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse multivariate polynomials with 'Semiring' instance.
--

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE TypeOperators    #-}

module Data.Poly.Multi.Semiring
  ( MultiPoly
  , VMultiPoly
  , UMultiPoly
  , unMultiPoly
  , toMultiPoly
  , monomial
  , scale
  , pattern X
  , pattern Y
  , pattern Z
  , eval
  , subst
  , deriv
  , integral
  , segregate
  , unsegregate
  ) where

import Data.Finite
import Data.Euclidean (Field)
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.TypeNats (KnownNat, type (<=))

import Data.Poly.Internal.Multi (MultiPoly, VMultiPoly, UMultiPoly, unMultiPoly, segregate, unsegregate)
import qualified Data.Poly.Internal.Multi as Multi
import Data.Poly.Internal.Multi.Field ()
import Data.Poly.Internal.Multi.GcdDomain ()

-- | Make 'MultiPoly' from a list of (powers, coefficient) pairs.
--
-- >>> :set -XOverloadedLists -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> toMultiPoly [(fromTuple (0,0),1),(fromTuple (0,1),2),(fromTuple (1,0),3)] :: VMultiPoly 2 Integer
-- 3 * X + 2 * Y + 1
-- >>> toMultiPoly [(fromTuple (0,0),0),(fromTuple (0,1),0),(fromTuple (1,0),0)] :: UMultiPoly 2 Int
-- 0
toMultiPoly
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => v (SU.Vector n Word, a)
  -> MultiPoly v n a
toMultiPoly = Multi.toMultiPoly'

-- | Create a monomial from powers and a coefficient.
monomial
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
monomial = Multi.monomial'

-- | Multiply a polynomial by a monomial, expressed as powers and a coefficient.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> scale (fromTuple (1, 1)) 3 (X^2 + Y) :: UMultiPoly 2 Int
-- 3 * X^3 * Y + 3 * X * Y^2
scale
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
  -> MultiPoly v n a
scale = Multi.scale'

-- | Create a polynomial equal to the first variable.
pattern X
  :: (Eq a, Semiring a, KnownNat n, 1 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern X = Multi.X'

-- | Create a polynomial equal to the second variable.
pattern Y
  :: (Eq a, Semiring a, KnownNat n, 2 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern Y = Multi.Y'

-- | Create a polynomial equal to the third variable.
pattern Z
  :: (Eq a, Semiring a, KnownNat n, 3 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern Z = Multi.Z'

-- | Evaluate at a given point.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> eval (X^2 + Y^2 :: UMultiPoly 2 Int) (fromTuple (3, 4) :: Data.Vector.Sized.Vector 2 Int)
-- 25
eval
  :: (Semiring a, G.Vector v (SU.Vector n Word, a), G.Vector u a)
  => MultiPoly v n a
  -> SG.Vector u n a
  -> a
eval = Multi.eval'

-- | Substitute another polynomials instead of variables.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> subst (X^2 + Y^2 + Z^2 :: UMultiPoly 3 Int) (fromTuple (X + 1, Y + 1, X + Y :: UMultiPoly 2 Int))
-- 2 * X^2 + 2 * X * Y + 2 * X + 2 * Y^2 + 2 * Y + 2
subst
  :: (Eq a, Semiring a, KnownNat m, G.Vector v (SU.Vector n Word, a), G.Vector w (SU.Vector m Word, a))
  => MultiPoly v n a
  -> SV.Vector n (MultiPoly w m a)
  -> MultiPoly w m a
subst = Multi.subst'

-- | Take a derivative with respect to the /i/-th variable.
--
-- >>> :set -XDataKinds
-- >>> deriv 0 (X^3 + 3 * Y) :: UMultiPoly 2 Int
-- 3 * X^2
-- >>> deriv 1 (X^3 + 3 * Y) :: UMultiPoly 2 Int
-- 3
deriv
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
deriv = Multi.deriv'

-- | Compute an indefinite integral of a polynomial
-- by the /i/-th variable,
-- setting constant term to zero.
--
-- >>> :set -XDataKinds
-- >>> integral 0 (3 * X^2 + 2 * Y) :: UMultiPoly 2 Double
-- 1.0 * X^3 + 2.0 * X * Y
-- >>> integral 1 (3 * X^2 + 2 * Y) :: UMultiPoly 2 Double
-- 3.0 * X^2 * Y + 1.0 * Y^2
integral
  :: (Field a, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
integral = Multi.integral'
