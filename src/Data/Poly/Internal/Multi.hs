-- |
-- Module:      Data.Poly.Internal.Multi
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--

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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Multi
  ( MultiPoly(..)
  , VMultiPoly
  , UMultiPoly
  , toMultiPoly
  , toMultiPoly'
  , leading
  , monomial
  , monomial'
  , scale
  , scale'
  , pattern X
  , pattern Y
  , pattern Z
  , pattern X'
  , pattern Y'
  , pattern Z'
  , eval
  , eval'
  , subst
  , subst'
  , substitute
  , substitute'
  , deriv
  , deriv'
  , integral
  , integral'
  -- * Univariate polynomials
  , Poly
  , VPoly
  , UPoly
  , unPoly
  -- * Conversions
  , segregate
  , unsegregate
  ) where

import Prelude hiding (quot, gcd)
import Control.Arrow
import Control.DeepSeq
import Data.Euclidean (Field, quot)
import Data.Finite
import Data.Kind
import Data.List (intersperse)
import Data.Semiring (Semiring(..), Ring())
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector.Sized as SV
import GHC.Exts (IsList(..))
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))

import Data.Poly.Internal.Multi.Core

-- | Sparse polynomials of @n@ variables with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use the patterns 'Data.Poly.Multi.X',
-- 'Data.Poly.Multi.Y' and
-- 'Data.Poly.Multi.Z' for construction:
--
-- >>> :set -XDataKinds
-- >>> (X + 1) + (Y - 1) + Z :: VMultiPoly 3 Integer
-- 1 * X + 1 * Y + 1 * Z
-- >>> (X + 1) * (Y - 1) :: UMultiPoly 2 Int
-- 1 * X * Y + (-1) * X + 1 * Y + (-1)
--
-- Polynomials are stored normalized, without
-- zero coefficients, so 0 * 'Data.Poly.Multi.X' + 1 equals to 1.
--
-- The 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
newtype MultiPoly (v :: Type -> Type) (n :: Nat) (a :: Type) = MultiPoly
  { unMultiPoly :: v (SU.Vector n Word, a)
  -- ^ Convert a 'MultiPoly' to a vector of (powers, coefficient) pairs.
  }

deriving instance Eq     (v (SU.Vector n Word, a)) => Eq     (MultiPoly v n a)
deriving instance Ord    (v (SU.Vector n Word, a)) => Ord    (MultiPoly v n a)
deriving instance NFData (v (SU.Vector n Word, a)) => NFData (MultiPoly v n a)

-- | Multivariate polynomials backed by boxed vectors.
type VMultiPoly (n :: Nat) (a :: Type) = MultiPoly V.Vector n a

-- | Multivariate polynomials backed by unboxed vectors.
type UMultiPoly (n :: Nat) (a :: Type) = MultiPoly U.Vector n a

-- | Sparse univariate polynomials with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use pattern 'Data.Poly.Multi.X' for construction:
--
-- >>> (X + 1) + (X - 1) :: VPoly Integer
-- 2 * X
-- >>> (X + 1) * (X - 1) :: UPoly Int
-- 1 * X^2 + (-1)
--
-- Polynomials are stored normalized, without
-- zero coefficients, so 0 * 'Data.Poly.Multi.X' + 1 equals to 1.
--
-- 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
type Poly (v :: Type -> Type) (a :: Type) = MultiPoly v 1 a

-- | Polynomials backed by boxed vectors.
type VPoly (a :: Type) = Poly V.Vector a

-- | Polynomials backed by unboxed vectors.
type UPoly (a :: Type) = Poly U.Vector a

-- | Convert a 'Poly' to a vector of coefficients.
unPoly
  :: (G.Vector v (Word, a), G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> v (Word, a)
unPoly = G.map (first SU.head) . unMultiPoly

instance (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a)) => IsList (MultiPoly v n a) where
  type Item (MultiPoly v n a) = (SU.Vector n Word, a)
  fromList = toMultiPoly' . G.fromList
  fromListN = (toMultiPoly' .) . G.fromListN
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
        = showsPrec 7 c . foldl (.) id
        ( map ((showString " * " .) . uncurry showPower)
        $ filter ((/= 0) . fst)
        $ zip (SU.toList is) (finites :: [Finite n]))

      -- Powers are guaranteed to be non-negative
      showPower :: Word -> Finite n -> String -> String
      showPower 1 n = showString (showVar n)
      showPower i n = showString (showVar n) . showString ("^" ++ show i)

      showVar :: Finite n -> String
      showVar = \case
        0 -> "X"
        1 -> "Y"
        2 -> "Z"
        k -> "X" ++ show k

-- | Make a 'MultiPoly' from a list of (powers, coefficient) pairs.
--
-- >>> :set -XOverloadedLists -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> toMultiPoly [(fromTuple (0,0),1),(fromTuple (0,1),2),(fromTuple (1,0),3)] :: VMultiPoly 2 Integer
-- 3 * X + 2 * Y + 1
-- >>> toMultiPoly [(fromTuple (0,0),0),(fromTuple (0,1),0),(fromTuple (1,0),0)] :: UMultiPoly 2 Int
-- 0
toMultiPoly
  :: (Eq a, Num a, G.Vector v (SU.Vector n Word, a))
  => v (SU.Vector n Word, a)
  -> MultiPoly v n a
toMultiPoly = MultiPoly . normalize (/= 0) (+)

toMultiPoly'
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => v (SU.Vector n Word, a)
  -> MultiPoly v n a
toMultiPoly' = MultiPoly . normalize (/= zero) plus

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

-- | Return the leading power and coefficient of a non-zero polynomial.
--
-- >>> import Data.Poly.Sparse (UPoly)
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: UPoly Int)
-- Just (3,4)
-- >>> leading (0 :: UPoly Int)
-- Nothing
leading :: G.Vector v (SU.Vector 1 Word, a) => Poly v a -> Maybe (Word, a)
leading (MultiPoly v)
  | G.null v  = Nothing
  | otherwise = Just $ first SU.head $ G.last v

-- | Multiply a polynomial by a monomial, expressed as powers and a coefficient.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> scale (fromTuple (1, 1)) 3 (X^2 + Y) :: UMultiPoly 2 Int
-- 3 * X^3 * Y + 3 * X * Y^2
scale
  :: (Eq a, Num a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
  -> MultiPoly v n a
scale yp yc = MultiPoly . scaleInternal (/= 0) (*) yp yc . unMultiPoly

scale'
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
  -> MultiPoly v n a
scale' yp yc = MultiPoly . scaleInternal (/= zero) times yp yc . unMultiPoly

-- | Create a monomial from powers and a coefficient.
monomial
  :: (Eq a, Num a, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
monomial p c
  | c == 0    = MultiPoly G.empty
  | otherwise = MultiPoly $ G.singleton (p, c)

monomial'
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
monomial' p c
  | c == zero = MultiPoly G.empty
  | otherwise = MultiPoly $ G.singleton (p, c)

-- | Evaluate the polynomial at a given point.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> eval (X^2 + Y^2 :: UMultiPoly 2 Int) (fromTuple (3, 4) :: Data.Vector.Sized.Vector 2 Int)
-- 25
eval
  :: (Num a, G.Vector v (SU.Vector n Word, a), G.Vector u a)
  => MultiPoly v n a
  -> SG.Vector u n a
  -> a
eval = substitute (*)
{-# INLINE eval #-}

eval'
  :: (Semiring a, G.Vector v (SU.Vector n Word, a), G.Vector u a)
  => MultiPoly v n a
  -> SG.Vector u n a
  -> a
eval' = substitute' times
{-# INLINE eval' #-}

-- | Substitute other polynomials instead of the variables.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> subst (X^2 + Y^2 + Z^2 :: UMultiPoly 3 Int) (fromTuple (X + 1, Y + 1, X + Y :: UMultiPoly 2 Int))
-- 2 * X^2 + 2 * X * Y + 2 * X + 2 * Y^2 + 2 * Y + 2
subst
  :: (Eq a, Num a, KnownNat m, G.Vector v (SU.Vector n Word, a), G.Vector w (SU.Vector m Word, a))
  => MultiPoly v n a
  -> SV.Vector n (MultiPoly w m a)
  -> MultiPoly w m a
subst = substitute (scale 0)
{-# INLINE subst #-}

subst'
  :: (Eq a, Semiring a, KnownNat m, G.Vector v (SU.Vector n Word, a), G.Vector w (SU.Vector m Word, a))
  => MultiPoly v n a
  -> SV.Vector n (MultiPoly w m a)
  -> MultiPoly w m a
subst' = substitute' (scale' 0)
{-# INLINE subst' #-}

substitute
  :: forall v u n a b.
     (G.Vector v (SU.Vector n Word, a), G.Vector u b, Num b)
  => (a -> b -> b)
  -> MultiPoly v n a
  -> SG.Vector u n b
  -> b
substitute f (MultiPoly cs) xs = G.foldl' go 0 cs
  where
    go :: b -> (SU.Vector n Word, a) -> b
    go acc (ps, c) = acc + f c (doMonom ps)

    doMonom :: SU.Vector n Word -> b
    doMonom = SU.ifoldl' (\acc i p -> acc * ((xs `SG.index` i) ^ p)) 1
{-# INLINE substitute #-}

substitute'
  :: forall v u n a b.
     (G.Vector v (SU.Vector n Word, a), G.Vector u b, Semiring b)
  => (a -> b -> b)
  -> MultiPoly v n a
  -> SG.Vector u n b
  -> b
substitute' f (MultiPoly cs) xs = G.foldl' go zero cs
  where
    go :: b -> (SU.Vector n Word, a) -> b
    go acc (ps, c) = acc `plus` f c (doMonom ps)

    doMonom :: SU.Vector n Word -> b
    doMonom = SU.ifoldl' (\acc i p -> acc `times` ((xs `SG.index` i) Semiring.^ p)) one
{-# INLINE substitute' #-}

-- | Take the derivative of the polynomial with respect to the /i/-th variable.
--
-- >>> :set -XDataKinds
-- >>> deriv 0 (X^3 + 3 * Y) :: UMultiPoly 2 Int
-- 3 * X^2
-- >>> deriv 1 (X^3 + 3 * Y) :: UMultiPoly 2 Int
-- 3
deriv
  :: (Eq a, Num a, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
deriv i (MultiPoly xs) = MultiPoly $ derivPoly
  (/= 0)
  (\ps -> ps SU.// [(i, ps `SU.index` i - 1)])
  (\ps c -> fromIntegral (ps `SU.index` i) * c)
  xs
{-# INLINE deriv #-}

deriv'
  :: (Eq a, Semiring a, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
deriv' i (MultiPoly xs) = MultiPoly $ derivPoly
  (/= zero)
  (\ps -> ps SU.// [(i, ps `SU.index` i - 1)])
  (\ps c -> fromNatural (fromIntegral (ps `SU.index` i)) `times` c)
  xs
{-# INLINE deriv' #-}

-- | Compute an indefinite integral of the polynomial
-- with respect to the /i/-th variable,
-- setting the constant term to zero.
--
-- >>> :set -XDataKinds
-- >>> integral 0 (3 * X^2 + 2 * Y) :: UMultiPoly 2 Double
-- 1.0 * X^3 + 2.0 * X * Y
-- >>> integral 1 (3 * X^2 + 2 * Y) :: UMultiPoly 2 Double
-- 3.0 * X^2 * Y + 1.0 * Y^2
integral
  :: (Fractional a, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
integral i (MultiPoly xs)
  = MultiPoly
  $ G.map (\(ps, c) -> let p = ps `SU.index` i in
    (ps SU.// [(i, p + 1)], c / fromIntegral (p + 1))) xs
{-# INLINE integral #-}

integral'
  :: (Field a, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
integral' i (MultiPoly xs)
  = MultiPoly
  $ G.map (\(ps, c) -> let p = ps `SU.index` i in
    (ps SU.// [(i, p + 1)], c `quot` Semiring.fromIntegral (p + 1))) xs
{-# INLINE integral' #-}

-- | Create a polynomial equal to the first variable.
pattern X
  :: (Eq a, Num a, KnownNat n, 1 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern X <- (isVar 0 -> True)
  where X = var 0

pattern X'
  :: (Eq a, Semiring a, KnownNat n, 1 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern X' <- (isVar' 0 -> True)
  where X' = var' 0

-- | Create a polynomial equal to the second variable.
pattern Y
  :: (Eq a, Num a, KnownNat n, 2 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern Y <- (isVar 1 -> True)
  where Y = var 1

pattern Y'
  :: (Eq a, Semiring a, KnownNat n, 2 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern Y' <- (isVar' 1 -> True)
  where Y' = var' 1

-- | Create a polynomial equal to the third variable.
pattern Z
  :: (Eq a, Num a, KnownNat n, 3 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern Z <- (isVar 2 -> True)
  where Z = var 2

pattern Z'
  :: (Eq a, Semiring a, KnownNat n, 3 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
pattern Z' <- (isVar' 2 -> True)
  where Z' = var' 2

var
  :: forall v n a.
     (Eq a, Num a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
var i
  | (1 :: a) == 0 = MultiPoly G.empty
  | otherwise     = MultiPoly $ G.singleton
    (SU.generate (\j -> if i == j then 1 else 0), 1)
{-# INLINE var #-}

var'
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
var' i
  | (one :: a) == zero = MultiPoly G.empty
  | otherwise          = MultiPoly $ G.singleton
    (SU.generate (\j -> if i == j then 1 else 0), one)
{-# INLINE var' #-}

isVar
  :: forall v n a.
     (Eq a, Num a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> Bool
isVar i (MultiPoly xs)
  | (1 :: a) == 0 = G.null xs
  | otherwise     = G.length xs == 1 && G.unsafeHead xs == (SU.generate (\j -> if i == j then 1 else 0), 1)
{-# INLINE isVar #-}

isVar'
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiPoly v n a
  -> Bool
isVar' i (MultiPoly xs)
  | (one :: a) == zero = G.null xs
  | otherwise          = G.length xs == 1 && G.unsafeHead xs == (SU.generate (\j -> if i == j then 1 else 0), one)
{-# INLINE isVar' #-}

-------------------------------------------------------------------------------

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
          mk = G.findIndex ((/= fy) . f) (G.unsafeTail xs)

-- | Interpret a multivariate polynomial over 1+/m/ variables
-- as a univariate polynomial, whose coefficients are
-- multivariate polynomials over the last /m/ variables.
segregate
  :: (G.Vector v (SU.Vector (1 + m) Word, a), G.Vector v (SU.Vector m Word, a))
  => MultiPoly v (1 + m) a
  -> VPoly (MultiPoly v m a)
segregate
  = MultiPoly
  . G.fromList
  . map (\vs -> (SU.take (fst (G.unsafeHead vs)), MultiPoly $ G.map (first SU.tail) vs))
  . groupOn (SU.head . fst)
  . unMultiPoly

-- | Interpret a univariate polynomials, whose coefficients are
-- multivariate polynomials over the first /m/ variables,
-- as a multivariate polynomial over 1+/m/ variables.
unsegregate
  :: (G.Vector v (SU.Vector (1 + m) Word, a), G.Vector v (SU.Vector m Word, a))
  => VPoly (MultiPoly v m a)
  -> MultiPoly v (1 + m) a
unsegregate
  = MultiPoly
  . G.concat
  . G.toList
  . G.map (\(v, MultiPoly vs) -> G.map (first (v SU.++)) vs)
  . unMultiPoly
