-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Polynomials.
--

{-# LANGUAGE PatternSynonyms     #-}

module Data.Poly
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
  -- * Semiring interface
  , toPoly'
  , constant'
  , pattern X'
  , eval'
  , deriv'
  ) where

import Data.Poly.Uni.Dense hiding (quotRem)
