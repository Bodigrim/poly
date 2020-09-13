-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Num'-based interface.
--

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
  , PolyOverField(..)
  , denseToSparse
  , sparseToDense
  ) where

import Data.Poly.Internal.Convert
import Data.Poly.Internal.Dense
import Data.Poly.Internal.Dense.Field ()
import Data.Poly.Internal.Dense.GcdDomain ()
import Data.Poly.Internal.PolyOverField
