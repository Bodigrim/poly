-- |
-- Module:      Data.Poly.Sparse.Multi
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints      #-}
#endif

module Data.Poly.Sparse.Multi
  ( MultiPoly(..)
  , VMultiPoly
  , UMultiPoly
  , toMultiPoly
  , monomial
  , scale
  , pattern X
  , pattern Y
  , pattern Z
  , eval
  , subst
  , deriv
  , integral
  ) where

import Prelude hiding (quot, gcd)
import Control.Arrow
import Control.DeepSeq
import Data.Euclidean (GcdDomain(..), Field, quot)
import Data.Finite
import Data.Kind
import Data.List (intersperse)
import Data.Proxy
import Data.Semiring (Semiring(..), Ring())
import qualified Data.Semiring as Semiring
import Data.Type.Equality
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.Exts (IsList(..))
import Unsafe.Coerce

import Data.Poly.Internal.Sparse (Poly(..), VPoly, toPoly', normalize, plusPoly, minusPoly, convolution, scaleInternal, derivPoly)
import Data.Poly.Internal.Sparse.GcdDomain ()

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (Nat, KnownNat, type (+), type (<=), SomeNat(..), natVal, someNatVal)
#else
import GHC.TypeLits (Nat, KnownNat, type (+), type (<=), SomeNat(..), natVal)
import qualified GHC.TypeLits as TL
import Data.Maybe

someNatVal :: Integer -> SomeNat
someNatVal = fromJust . TL.someNatVal
#endif

newtype MultiPoly (v :: Type -> Type) (n :: Nat) (a :: Type) = MultiPoly
  { unMultiPoly :: v (SU.Vector n Word, a)
  }

deriving instance Eq     (v (SU.Vector n Word, a)) => Eq     (MultiPoly v n a)
deriving instance Ord    (v (SU.Vector n Word, a)) => Ord    (MultiPoly v n a)
deriving instance NFData (v (SU.Vector n Word, a)) => NFData (MultiPoly v n a)

instance (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a)) => IsList (MultiPoly v n a) where
  type Item (MultiPoly v n a) = (SU.Vector n Word, a)
  fromList = toMultiPoly . G.fromList
  fromListN = (toMultiPoly .) . G.fromListN
  toList = G.toList . unMultiPoly

instance (Show a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Show (MultiPoly v n a) where
  showsPrec d (MultiPoly xs)
    | G.null xs
      = showString "0"
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.foldl (\acc (is, c) -> showCoeff is c : acc) [] xs
    where
      showCoeff is c
        = showsPrec 7 c . (foldl (.) id
        $ map ((showString " * " .) . uncurry showPower)
        $ filter ((/= 0) . fst)
        $ zip (SU.toList is) (finites :: [Finite n]))

      showPower :: Word -> Finite n -> String -> String
      showPower 1 n = showString (showVar n)
      showPower i n = showString (showVar n) . showString "^" . showsPrec 7 i

      showVar :: Finite n -> String
      showVar = \case
        0 -> "X"
        1 -> "Y"
        2 -> "Z"
        k -> "X" ++ show k

-- | Polynomials backed by boxed vectors.
type VMultiPoly n = MultiPoly V.Vector n

-- | Polynomials backed by unboxed vectors.
type UMultiPoly n = MultiPoly U.Vector n

toMultiPoly
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => v (SU.Vector n Word, a)
  -> MultiPoly v n a
toMultiPoly = MultiPoly . normalize (/= zero) plus

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Num (MultiPoly v n a) where
  MultiPoly xs + MultiPoly ys = MultiPoly $ plusPoly (/= 0) (+) xs ys
  MultiPoly xs - MultiPoly ys = MultiPoly $ minusPoly (/= 0) negate (-) xs ys
  negate (MultiPoly xs) = MultiPoly $ G.map (fmap negate) xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> MultiPoly G.empty
    m -> MultiPoly $ G.singleton (0, m)
  MultiPoly xs * MultiPoly ys = MultiPoly $ convolution (/= 0) (+) (*) xs ys
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Semiring (MultiPoly v n a) where
  zero = MultiPoly G.empty
  one
    | (one :: a) == zero = zero
    | otherwise = MultiPoly $ G.singleton (0, one)
  plus (MultiPoly xs) (MultiPoly ys) = MultiPoly $ plusPoly (/= zero) plus xs ys
  times (MultiPoly xs) (MultiPoly ys) = MultiPoly $ convolution (/= zero) plus times xs ys
  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}

  fromNatural n = if n' == zero then zero else MultiPoly $ G.singleton (0, n')
    where
      n' :: a
      n' = fromNatural n
  {-# INLINE fromNatural #-}

instance (Eq a, Ring a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Ring (MultiPoly v n a) where
  negate (MultiPoly xs) = MultiPoly $ G.map (fmap Semiring.negate) xs

scale
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
  -> MultiPoly v n a
scale yp yc = MultiPoly . scaleInternal (/= zero) times yp yc . unMultiPoly

monomial
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
monomial p c
  | c == zero = MultiPoly G.empty
  | otherwise = MultiPoly $ G.singleton (p, c)

eval
  :: (Semiring a, G.Vector v (SU.Vector n Word, a), G.Vector u a)
  => MultiPoly v n a
  -> SG.Vector u n a
  -> a
eval = substitute times
{-# INLINE eval #-}

subst
  :: (Eq a, Semiring a, KnownNat m, G.Vector v (SU.Vector n Word, a), G.Vector w (SU.Vector m Word, a), G.Vector u (MultiPoly w m a))
  => MultiPoly v n a
  -> SG.Vector u n (MultiPoly w m a)
  -> MultiPoly w m a
subst = substitute (scale 0)
{-# INLINE subst #-}

substitute
  :: forall v u n a b.
     (G.Vector v (SU.Vector n Word, a), G.Vector u b, Semiring b)
  => (a -> b -> b)
  -> MultiPoly v n a
  -> SG.Vector u n b
  -> b
substitute f (MultiPoly cs) xs = G.foldl' go zero cs
  where
    go :: b -> (SU.Vector n Word, a) -> b
    go acc (ps, c) = acc `plus` f c (doMonom ps)

    doMonom :: SU.Vector n Word -> b
    doMonom = SU.ifoldl' (\acc i p -> acc `times` ((xs `SG.index` i) Semiring.^ p)) one
{-# INLINE substitute #-}

deriv
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a), KnownNat n)
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
deriv i (MultiPoly xs) = MultiPoly $ derivPoly
  (/= zero)
  -- Does it spoil grevlex ordering?
  (\ps -> ps SU.// [(i, ps `SU.index` i - 1)])
  (\ps c -> fromNatural (fromIntegral (ps `SU.index` i)) `times` c)
  xs

integral
  :: (Eq a, Field a, G.Vector v (SU.Vector n Word, a), KnownNat n)
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
integral i (MultiPoly xs)
  = MultiPoly
  $ G.map (\(ps, c) -> let p = ps `SU.index` i in
    -- Does it spoil grevlex ordering?
    (ps SU.// [(i, p + 1)], c `quot` Semiring.fromIntegral (p + 1))) xs

pattern X
  :: (Eq a, Semiring a, KnownNat n, 1 <= n, G.Vector v (SU.Vector n Word, a), Eq (v (SU.Vector n Word, a)))
  => MultiPoly v n a
pattern X <- ((==) (var 0) -> True)
  where X = var 0

pattern Y
  :: (Eq a, Semiring a, KnownNat n, 2 <= n, G.Vector v (SU.Vector n Word, a), Eq (v (SU.Vector n Word, a)))
  => MultiPoly v n a
pattern Y <- ((==) (var 1) -> True)
  where Y = var 1

pattern Z
  :: (Eq a, Semiring a, KnownNat n, 3 <= n, G.Vector v (SU.Vector n Word, a), Eq (v (SU.Vector n Word, a)))
  => MultiPoly v n a
pattern Z <- ((==) (var 2) -> True)
  where Z = var 2

var
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a), Eq (v (SU.Vector n Word, a)))
  => Finite n
  -> MultiPoly v n a
var i
  | (one :: a) == zero = MultiPoly G.empty
  | otherwise          = MultiPoly $ G.singleton
    (SU.generate (\j -> if i == j then 1 else 0), one)
{-# INLINE var #-}

-------------------------------------------------------------------------------
-- GcdDomain

data IsZeroOrSucc n where
  IsZero :: n :~: 0 -> IsZeroOrSucc n
  IsSucc :: KnownNat m => n :~: 1 + m -> IsZeroOrSucc n

isZeroOrSucc :: forall n. KnownNat n => IsZeroOrSucc n
isZeroOrSucc = case natVal (Proxy :: Proxy n) of
  0 -> IsZero (unsafeCoerce Refl)
  n -> case someNatVal (n - 1) of
    SomeNat (_ :: Proxy m) -> IsSucc (unsafeCoerce Refl :: n :~: 1 + m)

trivial :: (Semiring a, G.Vector v (SU.Vector 0 Word, a)) => MultiPoly v 0 a -> a
trivial (MultiPoly xs)
  | G.null xs = zero
  | otherwise = snd (G.head xs)

untrivial :: (Eq a, Semiring a, G.Vector v (SU.Vector 0 Word, a)) => a -> MultiPoly v 0 a
untrivial x
  | x == zero = MultiPoly G.empty
  | otherwise = MultiPoly $ G.singleton (SU.empty, x)

groupOn :: (G.Vector v a, Eq b) => (a -> b) -> v a -> [v a]
groupOn f = go
  where
    go xs
      | G.null xs = []
      | otherwise = case mk of
        Nothing -> [xs]
        Just k  -> G.unsafeTake (k + 1) xs : go (G.unsafeDrop (k + 1) xs)
        where
          fy = f (G.unsafeHead xs)
          mk = G.findIndex (not . (== fy) . f) (G.unsafeTail xs)

separate :: (KnownNat m, n ~ (1 + m), Eq a, Semiring a, Eq (v (SU.Vector m Word, a)), G.Vector v (SU.Vector n Word, a), G.Vector v (SU.Vector m Word, a)) => MultiPoly v n a -> VPoly (MultiPoly v m a)
separate (MultiPoly xs) = toPoly' $ G.fromList $ map (\vs -> (SU.head (fst (G.unsafeHead vs)), MultiPoly $ G.map (first SU.tail) vs)) $ groupOn (SU.head . fst) xs

unseparate :: (n ~ (1 + m), G.Vector v (SU.Vector n Word, a), G.Vector v (SU.Vector m Word, a)) => VPoly (MultiPoly v m a) -> MultiPoly v n a
unseparate (Poly xs) = MultiPoly $ G.concat $ G.toList $ G.map (\(v, MultiPoly vs) -> G.map (first (SU.cons v)) vs) xs

#if __GLASGOW_HASKELL__ >= 806
instance (Eq a, Ring a, GcdDomain a, KnownNat n, forall m. KnownNat m => G.Vector v (SU.Vector m Word, a), forall m. KnownNat m => Eq (v (SU.Vector m Word, a))) => GcdDomain (MultiPoly v n a) where
#else
instance (Eq a, Ring a, GcdDomain a, KnownNat n) => GcdDomain (VMultiPoly n a) where
#endif
  divide xs ys = case isZeroOrSucc :: IsZeroOrSucc n of
    IsZero Refl -> untrivial  <$> trivial  xs `divide` trivial  ys
    IsSucc Refl -> unseparate <$> separate xs `divide` separate ys
  gcd xs ys
    | G.null (unMultiPoly xs) = ys
    | G.null (unMultiPoly ys) = xs
    | G.length (unMultiPoly xs) == 1 = gcdSingleton (G.unsafeHead (unMultiPoly xs)) ys
    | G.length (unMultiPoly ys) == 1 = gcdSingleton (G.unsafeHead (unMultiPoly ys)) xs
    | otherwise = case isZeroOrSucc :: IsZeroOrSucc n of
      IsZero Refl -> untrivial  $ trivial  xs `gcd` trivial  ys
      IsSucc Refl -> unseparate $ separate xs `gcd` separate ys

gcdSingleton :: (Eq a, GcdDomain a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => (SU.Vector n Word, a) -> MultiPoly v n a -> MultiPoly v n a
gcdSingleton pc (MultiPoly pcs) = uncurry monomial $
  G.foldl' (\(accP, accC) (p, c) -> (SU.zipWith min accP p, gcd accC c)) pc pcs
