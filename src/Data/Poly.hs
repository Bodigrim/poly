-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Num'-based interface.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

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
  -- * Fractional coefficients
  , PolyOverFractional(..)
  ) where

import Data.Poly.Uni.Dense
import Data.Poly.Uni.Dense.Fractional ()
import Data.Poly.Uni.Dense.GcdDomain ()
import Data.Poly.Uni.PolyOverFractional
