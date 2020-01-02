-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Num'-based interface.
--

{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Poly
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
  , subst
  , deriv
  , integral
#if MIN_VERSION_semirings(0,4,2)
  -- * Polynomials over 'Field'
  , PolyOverField(..)
  , PolyOverFractional
  , pattern PolyOverFractional
  , unPolyOverFractional
#endif
  -- * Laurent interface
  , Laurent
  , VLaurent
  , ULaurent
  , toLaurent
  ) where

import           Data.Poly.Internal.Dense
import           Data.Poly.Internal.Dense.Laurent   (Laurent, ULaurent,
                                                     VLaurent, toLaurent)
#if MIN_VERSION_semirings(0,4,2)
import           Data.Poly.Internal.Dense.Field     ()
import           Data.Poly.Internal.Dense.GcdDomain ()
import           Data.Poly.Internal.PolyOverField
#endif
