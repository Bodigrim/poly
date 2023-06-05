{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Data.Poly.Internal.Multi.Monom 
  ( Monom(..)
  ) where

-- import Control.DeepSeq
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.Generics (Generic)
import GHC.TypeNats (KnownNat)

-- | A strict tuple of a vector of unsigned powers and a coefficient.
-- Basic building block for sparse polynomials.
--
-- In the current API 'Monom' rarely appears as an argument,
-- only in constraints.
data Monom n a = Monom
  { monomPower :: !(SU.Vector n Word)
  , monomCoeff :: !a
  } deriving (Generic)

instance U.IsoUnbox (Monom n a) (SU.Vector n Word, a)

newtype instance MU.MVector s (Monom n a) =
  MV_Monom (MU.MVector s (SU.Vector n Word, a))

newtype instance U.Vector (Monom n a) =
  V_Monom  (U.Vector (SU.Vector n Word, a))

deriving via (Monom n a `U.As` (SU.Vector n Word, a))
  instance (KnownNat n, U.Unbox a) => MG.MVector U.MVector (Monom n a)

deriving via (Monom n a `U.As` (SU.Vector n Word, a))
  instance (KnownNat n, U.Unbox a) => G.Vector  U.Vector  (Monom n a)

instance (KnownNat n, U.Unbox a) => U.Unbox (Monom n a)
