-- |
-- Module:      Data.Poly
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials and a 'Num'-based interface.
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}

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
  , deriv
  , integral
#if MIN_VERSION_semirings(0,4,2)
  , gcdExt
  -- * Fractional coefficients
  , PolyOverFractional(..)
  , fractionalGcdExt
  , scaleMonic
#endif
  ) where

import Data.Poly.Internal.Dense
#if MIN_VERSION_semirings(0,4,2)
import Data.Poly.Internal.Dense.Fractional ()
import Data.Poly.Internal.Dense.GcdDomain ()
import Data.Poly.Internal.PolyOverFractional
#endif
