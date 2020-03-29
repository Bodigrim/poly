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
{-# LANGUAGE PatternSynonyms            #-}

module Data.Poly.Internal.PolyOverField
  ( PolyOverField(..)
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm, (^))
import Control.DeepSeq (NFData)
import Data.Euclidean
import Data.Semiring
import qualified Data.Vector.Generic as G

import qualified Data.Poly.Internal.Dense as Dense
import qualified Data.Poly.Internal.Dense.Field as Dense (fieldGcd)

-- | Wrapper for polynomials over 'Field',
-- providing a faster 'GcdDomain' instance.
newtype PolyOverField poly = PolyOverField { unPolyOverField :: poly }
  deriving (Eq, NFData, Num, Ord, Ring, Semiring, Show)

instance (Eq a, Eq (v a), Field a, G.Vector v a) => GcdDomain (PolyOverField (Dense.Poly v a)) where
  gcd (PolyOverField x) (PolyOverField y) = PolyOverField (Dense.fieldGcd x y)
  {-# INLINE gcd #-}

instance (Eq a, Eq (v a), Field a, G.Vector v a) => Euclidean (PolyOverField (Dense.Poly v a)) where
  degree (PolyOverField x) =
    degree x
  quotRem (PolyOverField x) (PolyOverField y) =
    let (q, r) = quotRem x y in
      (PolyOverField q, PolyOverField r)
  {-# INLINE quotRem #-}
  rem (PolyOverField x) (PolyOverField y) =
    PolyOverField (rem x y)
  {-# INLINE rem #-}
