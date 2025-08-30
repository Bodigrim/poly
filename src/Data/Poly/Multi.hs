-- |
-- Module:      Data.Poly.Multi
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse multivariate polynomials with 'Num' instance.
--
-- @since 0.5.0.0

{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms  #-}

{-# OPTIONS_GHC -Wno-unrecognised-warning-flags -Wno-pattern-namespace-specifier #-}

module Data.Poly.Multi
  ( MultiPoly
  , VMultiPoly
  , UMultiPoly
  , Monom
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

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU

import Data.Poly.Internal.Multi hiding (unMultiPoly, toMultiPoly)
import qualified Data.Poly.Internal.Multi as Multi
import Data.Poly.Internal.Multi.Field ()
import Data.Poly.Internal.Multi.GcdDomain ()

-- | Make a 'MultiPoly' from a list of (powers, coefficient) pairs.
--
-- >>> :set -XOverloadedLists -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> toMultiPoly [(fromTuple (0,0),1),(fromTuple (0,1),2),(fromTuple (1,0),3)] :: VMultiPoly 2 Integer
-- 3 * X + 2 * Y + 1
-- >>> toMultiPoly [(fromTuple (0,0),0),(fromTuple (0,1),0),(fromTuple (1,0),0)] :: UMultiPoly 2 Int
-- 0
--
-- @since 0.5.0.0
toMultiPoly
  :: (Eq a, Num a, G.Vector v (Monom n a), G.Vector v (SU.Vector n Word, a))
  => v (SU.Vector n Word, a)
  -> MultiPoly v n a
toMultiPoly = Multi.toMultiPoly . G.map (uncurry Monom)

-- | Convert a 'MultiPoly' to a vector of (powers, coefficient) pairs.
--
-- @since 0.5.0.0
unMultiPoly
  :: (G.Vector v (Monom n a), G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
  -> v (SU.Vector n Word, a)
unMultiPoly = G.map (\(Monom p c) -> (p, c)) . Multi.unMultiPoly
