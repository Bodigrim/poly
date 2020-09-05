-- |
-- Module:      Data.Poly.Internal.PolyOverField
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Wrapper with a more efficient 'Euclidean' instance.
--

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Poly.Internal.PolyOverField
  ( PolyOverField(..)
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm, (^))
import Control.DeepSeq (NFData)
import Data.Euclidean
import Data.Semiring

-- | Wrapper for polynomials.
newtype PolyOverField poly = PolyOverField { unPolyOverField :: poly }
  deriving (Eq, NFData, Num, Ord, Ring, Semiring, GcdDomain, Euclidean, Show)
{-# DEPRECATED PolyOverField "Does not provide performance benefits anymore" #-}
