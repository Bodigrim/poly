-- |
-- Module:      Quaternion
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- This is a toy implementtion of quaternions,
-- serving solely to test polynomials
-- over non-commutative rings.
--

{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Quaternion
  ( Quaternion(..)
  ) where

import Prelude hiding (negate)
import Data.Semiring (Semiring(..), Ring(..), minus)
import GHC.Generics
import Test.Tasty.QuickCheck hiding (scale)

import Data.Vector.Unboxed (Vector, Unbox)
import qualified Data.Vector.Generic as G
import Data.Vector.Unboxed.Mutable (MVector)
import qualified Data.Vector.Generic.Mutable as M

data Quaternion a = Quaternion !a !a !a !a
  deriving (Eq, Ord, Show, Generic)

instance Ring a => Semiring (Quaternion a) where
  zero = Quaternion zero zero zero zero
  one  = Quaternion  one zero zero zero
  plus (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) =
    Quaternion (a1 `plus` a2) (b1 `plus` b2) (c1 `plus` c2) (d1 `plus` d2)
  times (Quaternion a1 b1 c1 d1) (Quaternion a2 b2 c2 d2) =
    Quaternion
      (a1 `times` a2 `minus` b1 `times` b2 `minus` c1 `times` c2 `minus` d1 `times` d2)
      (a1 `times` b2  `plus` b1 `times` a2  `plus` c1 `times` d2 `minus` d1 `times` c2)
      (a1 `times` c2 `minus` b1 `times` d2  `plus` c1 `times` a2  `plus` d1 `times` b2)
      (a1 `times` d2  `plus` b1 `times` c2 `minus` c1 `times` b2  `plus` d1 `times` a2)

instance Ring a => Ring (Quaternion a) where
  negate (Quaternion a b c d) =
    Quaternion (negate a) (negate b) (negate c) (negate d)

instance (Ring a, Num a) => Num (Quaternion a) where
  (+) = plus
  (-) = minus
  (*) = times
  abs = id
  signum = const one
  fromInteger n = Quaternion (fromInteger n) zero zero zero

instance Arbitrary a => Arbitrary (Quaternion a) where
  arbitrary = Quaternion <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

newtype instance MVector s (Quaternion a) = MV_Quaternion (MVector s (a, a, a, a))
newtype instance Vector    (Quaternion a) = V_Quaternion  (Vector    (a, a, a, a))

instance (Unbox a) => Unbox (Quaternion a)

instance (Unbox a) => M.MVector MVector (Quaternion a) where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Quaternion v) = M.basicLength v
  basicUnsafeSlice i n (MV_Quaternion v) = MV_Quaternion $ M.basicUnsafeSlice i n v
  basicOverlaps (MV_Quaternion v1) (MV_Quaternion v2) = M.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Quaternion `fmap` M.basicUnsafeNew n
  basicInitialize (MV_Quaternion v) = M.basicInitialize v
  basicUnsafeReplicate n (Quaternion a b c d) = MV_Quaternion `fmap` M.basicUnsafeReplicate n (a, b, c, d)
  basicUnsafeRead (MV_Quaternion v) i = (\(a, b, c, d) -> Quaternion a b c d) `fmap` M.basicUnsafeRead v i
  basicUnsafeWrite (MV_Quaternion v) i (Quaternion a b c d) = M.basicUnsafeWrite v i (a, b, c, d)
  basicClear (MV_Quaternion v) = M.basicClear v
  basicSet (MV_Quaternion v) (Quaternion a b c d) = M.basicSet v (a, b, c, d)
  basicUnsafeCopy (MV_Quaternion v1) (MV_Quaternion v2) = M.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Quaternion v1) (MV_Quaternion v2) = M.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Quaternion v) n = MV_Quaternion `fmap` M.basicUnsafeGrow v n

instance (Unbox a) => G.Vector Vector (Quaternion a) where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Quaternion v) = V_Quaternion `fmap` G.basicUnsafeFreeze v
  basicUnsafeThaw (V_Quaternion v) = MV_Quaternion `fmap` G.basicUnsafeThaw v
  basicLength (V_Quaternion v) = G.basicLength v
  basicUnsafeSlice i n (V_Quaternion v) = V_Quaternion $ G.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Quaternion v) i
                = (\(a, b, c, d) -> Quaternion a b c d) `fmap` G.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Quaternion mv) (V_Quaternion v)
                = G.basicUnsafeCopy mv v
  elemseq _ (Quaternion a b c d) z = G.elemseq (undefined :: Vector a) a
                                   $ G.elemseq (undefined :: Vector a) b
                                   $ G.elemseq (undefined :: Vector a) c
                                   $ G.elemseq (undefined :: Vector a) d z
