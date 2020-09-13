-- |
-- Module:      Data.Poly.Sparse
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Num' instance.
--

{-# LANGUAGE PatternSynonyms #-}

module Data.Poly.Sparse
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
  -- * Conversions
  , denseToSparse
  , sparseToDense
  ) where

import Data.Poly.Internal.Convert
import Data.Poly.Internal.Sparse
import Data.Poly.Internal.Sparse.Field ()
import Data.Poly.Internal.Sparse.GcdDomain ()
