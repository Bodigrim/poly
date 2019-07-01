-- |
-- Module:      Data.Poly.Sparse
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse polynomials with 'Num' instance.
--

{-# LANGUAGE PatternSynonyms     #-}

module Data.Poly.Sparse
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  -- * Num interface
  , toPoly
  , constant
  , pattern X
  , eval
  , deriv
  , integral
  -- * Fractional coefficients
  , PolyOverFractional(..)
  ) where

import Data.Poly.Uni.Sparse
import Data.Poly.Uni.Sparse.Fractional ()
import Data.Poly.Uni.Sparse.GcdDomain ()
import Data.Poly.Uni.PolyOverFractional
