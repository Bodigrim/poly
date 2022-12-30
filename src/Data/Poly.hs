-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Num'-based interface.
--
-- @since 0.1.0.0
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Poly
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
  , quotRemFractional
#ifdef SupportSparse
  , denseToSparse
  , sparseToDense
#endif
  ) where

#ifdef SupportSparse
import Data.Poly.Internal.Convert
#endif
import Data.Poly.Internal.Dense
import Data.Poly.Internal.Dense.Field (quotRemFractional)
import Data.Poly.Internal.Dense.GcdDomain ()
