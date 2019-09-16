-- |
-- Module:      Data.Poly.Sparse
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Num' instance.
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Poly.Sparse
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  , leading
  -- * Num interface
  , toPoly
  , monomial
  , scale
  , pattern X
  , eval
  , deriv
  , integral
#if MIN_VERSION_semirings(0,4,2)
  -- * Polynomials over 'Field'
  , gcdExt
#endif
  ) where

import Data.Poly.Internal.Sparse
#if MIN_VERSION_semirings(0,4,2)
import Data.Poly.Internal.Sparse.Field (gcdExt)
import Data.Poly.Internal.Sparse.GcdDomain ()
#endif
