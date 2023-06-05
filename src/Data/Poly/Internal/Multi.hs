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
{-# LANGUAGE TypeApplications           #-}
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
  -- * Monom
  , Monom(..)
  ) where

import Prelude hiding (quot, gcd)
import Control.DeepSeq
import Data.Coerce
import Data.Euclidean (Field, quot)
import Data.Finite
import Data.Function (on)
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
-- Due to being polymorphic by multiple axis, the performance of `MultiPoly` crucially
-- depends on specialisation of instances. Clients are strongly recommended
-- to compile with @ghc-options:@ @-fspecialise-aggressively@ and suggested to enable @-O2@.
--
-- @since 0.5.0.0
newtype MultiPoly (v :: Type -> Type) (n :: Nat) (a :: Type) = MultiPoly
  { unMultiPoly :: v (Monom n a)
  }

instance (Eq a, G.Vector v (Monom n a)) => Eq (MultiPoly v n a) where
  (==) = G.eqBy (\(Monom p c) (Monom p' c') -> c == c' && p == p') `on` unMultiPoly

instance (Ord a, G.Vector v (Monom n a)) => Ord (MultiPoly v n a) where
  compare = G.cmpBy (\(Monom p c) (Monom p' c') -> compare c c' <> compare p p') `on` unMultiPoly

instance (NFData a, NFData1 v) => NFData (MultiPoly v n a) where
  rnf = liftRnf (\(Monom _ a) -> rnf a) . unMultiPoly

-- | Multivariate polynomials backed by boxed vectors.
--
-- @since 0.5.0.0
type VMultiPoly (n :: Nat) (a :: Type) = MultiPoly V.Vector n a

-- | Multivariate polynomials backed by unboxed vectors.
--
-- @since 0.5.0.0
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
-- Due to being polymorphic by multiple axis, the performance of `Poly` crucially
-- depends on specialisation of instances. Clients are strongly recommended
-- to compile with @ghc-options:@ @-fspecialise-aggressively@ and suggested to enable @-O2@.
--
-- @since 0.3.0.0
type Poly (v :: Type -> Type) (a :: Type) = MultiPoly v 1 a

-- | Polynomials backed by boxed vectors.
--
-- @since 0.3.0.0
type VPoly (a :: Type) = Poly V.Vector a

-- | Polynomials backed by unboxed vectors.
--
-- @since 0.3.0.0
type UPoly (a :: Type) = Poly U.Vector a

-- | Convert a 'Poly' to a vector of coefficients.
--
-- @since 0.3.0.0
unPoly
  :: (G.Vector v (Word, a), G.Vector v (Monom 1 a))
  => Poly v a
  -> v (Word, a)
unPoly = G.map (\(Monom p c) -> (SU.head p, c)) . unMultiPoly

instance (Eq a, Semiring a, G.Vector v (Monom n a)) => IsList (MultiPoly v n a) where
  type Item (MultiPoly v n a) = (SU.Vector n Word, a)
  fromList = toMultiPoly' . G.fromList . map (uncurry Monom)
  fromListN = (toMultiPoly' .) . (. map (uncurry Monom)) . G.fromListN
  toList = map (\(Monom p c) -> (p, c)) . G.toList . unMultiPoly

instance (Show a, KnownNat n, G.Vector v (Monom n a)) => Show (MultiPoly v n a) where
  showsPrec d (MultiPoly xs)
    | G.null xs
      = showString "0"
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.foldl (\acc (Monom is c) -> showCoeff is c : acc) [] xs
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

toMultiPoly
  :: (Eq a, Num a, G.Vector v (Monom n a))
  => v (Monom n a)
  -> MultiPoly v n a
toMultiPoly = MultiPoly . normalize (/= 0) (+)
{-# INLINABLE toMultiPoly #-}

toMultiPoly'
  :: (Eq a, Semiring a, G.Vector v (Monom n a))
  => v (Monom n a)
  -> MultiPoly v n a
toMultiPoly' = MultiPoly . normalize (/= zero) plus
{-# INLINABLE toMultiPoly' #-}

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, KnownNat n, G.Vector v (Monom n a)) => Num (MultiPoly v n a) where

  (+) = coerce (plusPoly    @v @n @a (/= 0) (+))
  (-) = coerce (minusPoly   @v @n @a (/= 0) negate (-))
  (*) = coerce (convolution @v @n @a (/= 0) (+) (*))

  negate (MultiPoly xs) = MultiPoly $ G.map (\(Monom ps c) -> Monom ps (negate c)) xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> MultiPoly G.empty
    m -> MultiPoly $ G.singleton (Monom 0 m)

  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Semiring a, KnownNat n, G.Vector v (Monom n a)) => Semiring (MultiPoly v n a) where
  zero = MultiPoly G.empty
  one
    | (one :: a) == zero = zero
    | otherwise = MultiPoly $ G.singleton (Monom 0 one)

  plus  = coerce (plusPoly    @v @n @a (/= zero) plus)
  times = coerce (convolution @v @n @a (/= zero) plus times)

  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}

  fromNatural n = if n' == zero then zero else MultiPoly $ G.singleton (Monom 0 n')
    where
      n' :: a
      n' = fromNatural n
  {-# INLINE fromNatural #-}

instance (Eq a, Ring a, KnownNat n, G.Vector v (Monom n a)) => Ring (MultiPoly v n a) where
  negate (MultiPoly xs) = MultiPoly $ G.map (\(Monom ps c) -> Monom ps (Semiring.negate c)) xs

-- | Return the leading power and coefficient of a non-zero polynomial.
--
-- >>> import Data.Poly.Sparse (UPoly)
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: UPoly Int)
-- Just (3,4)
-- >>> leading (0 :: UPoly Int)
-- Nothing
--
-- @since 0.3.0.0
leading :: G.Vector v (Monom 1 a) => Poly v a -> Maybe (Word, a)
leading (MultiPoly v)
  | G.null v  = Nothing
  | otherwise = Just (SU.head p, c)
    where
      Monom p c = G.last v

-- | Multiply a polynomial by a monomial, expressed as powers and a coefficient.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> scale (fromTuple (1, 1)) 3 (X^2 + Y) :: UMultiPoly 2 Int
-- 3 * X^3 * Y + 3 * X * Y^2
--
-- @since 0.5.0.0
scale
  :: (Eq a, Num a, KnownNat n, G.Vector v (Monom n a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
  -> MultiPoly v n a
scale yp yc = MultiPoly . scaleInternal (/= 0) (*) yp yc . unMultiPoly

scale'
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (Monom n a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
  -> MultiPoly v n a
scale' yp yc = MultiPoly . scaleInternal (/= zero) times yp yc . unMultiPoly

-- | Create a monomial from powers and a coefficient.
--
-- @since 0.5.0.0
monomial
  :: (Eq a, Num a, G.Vector v (Monom n a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
monomial p c
  | c == 0    = MultiPoly G.empty
  | otherwise = MultiPoly $ G.singleton (Monom p c)
{-# INLINABLE monomial #-}

monomial'
  :: (Eq a, Semiring a, G.Vector v (Monom n a))
  => SU.Vector n Word
  -> a
  -> MultiPoly v n a
monomial' p c
  | c == zero = MultiPoly G.empty
  | otherwise = MultiPoly $ G.singleton (Monom p c)
{-# INLINABLE monomial' #-}

-- | Evaluate the polynomial at a given point.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> eval (X^2 + Y^2 :: UMultiPoly 2 Int) (fromTuple (3, 4) :: Data.Vector.Sized.Vector 2 Int)
-- 25
--
-- @since 0.5.0.0
eval
  :: (Num a, G.Vector v (Monom n a), G.Vector u a)
  => MultiPoly v n a
  -> SG.Vector u n a
  -> a
eval = substitute (*)
{-# INLINE eval #-}

eval'
  :: (Semiring a, G.Vector v (Monom n a), G.Vector u a)
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
--
-- @since 0.5.0.0
subst
  :: (Eq a, Num a, KnownNat m, G.Vector v (Monom n a), G.Vector w (Monom m a))
  => MultiPoly v n a
  -> SV.Vector n (MultiPoly w m a)
  -> MultiPoly w m a
subst = substitute (scale 0)
{-# INLINE subst #-}

subst'
  :: (Eq a, Semiring a, KnownNat m, G.Vector v (Monom n a), G.Vector w (Monom m a))
  => MultiPoly v n a
  -> SV.Vector n (MultiPoly w m a)
  -> MultiPoly w m a
subst' = substitute' (scale' 0)
{-# INLINE subst' #-}

substitute
  :: forall v u n a b.
     (G.Vector v (Monom n a), G.Vector u b, Num b)
  => (a -> b -> b)
  -> MultiPoly v n a
  -> SG.Vector u n b
  -> b
substitute f (MultiPoly cs) xs = G.foldl' go 0 cs
  where
    go :: b -> (Monom n a) -> b
    go acc (Monom ps c) = acc + f c (doMonom ps)

    doMonom :: SU.Vector n Word -> b
    doMonom = SU.ifoldl' (\acc i p -> acc * ((xs `SG.index` i) ^ p)) 1
{-# INLINE substitute #-}

substitute'
  :: forall v u n a b.
     (G.Vector v (Monom n a), G.Vector u b, Semiring b)
  => (a -> b -> b)
  -> MultiPoly v n a
  -> SG.Vector u n b
  -> b
substitute' f (MultiPoly cs) xs = G.foldl' go zero cs
  where
    go :: b -> (Monom n a) -> b
    go acc (Monom ps c) = acc `plus` f c (doMonom ps)

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
--
-- @since 0.5.0.0
deriv
  :: (Eq a, Num a, G.Vector v (Monom n a))
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
  :: (Eq a, Semiring a, G.Vector v (Monom n a))
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
--
-- @since 0.5.0.0
integral
  :: (Fractional a, G.Vector v (Monom n a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
integral i (MultiPoly xs)
  = MultiPoly
  $ G.map (\(Monom ps c) -> let p = ps `SU.index` i in
    (Monom (ps SU.// [(i, p + 1)]) (c / fromIntegral (p + 1)))) xs
{-# INLINE integral #-}

integral'
  :: (Field a, G.Vector v (Monom n a))
  => Finite n
  -> MultiPoly v n a
  -> MultiPoly v n a
integral' i (MultiPoly xs)
  = MultiPoly
  $ G.map (\(Monom ps c) -> let p = ps `SU.index` i in
    (Monom (ps SU.// [(i, p + 1)]) (c `quot` Semiring.fromIntegral (p + 1)))) xs
{-# INLINE integral' #-}

-- | Create a polynomial equal to the first variable.
--
-- @since 0.5.0.0
pattern X
  :: (Eq a, Num a, KnownNat n, 1 <= n, G.Vector v (Monom n a))
  => MultiPoly v n a
pattern X <- (isVar 0 -> True)
  where X = var 0

pattern X'
  :: (Eq a, Semiring a, KnownNat n, 1 <= n, G.Vector v (Monom n a))
  => MultiPoly v n a
pattern X' <- (isVar' 0 -> True)
  where X' = var' 0

-- | Create a polynomial equal to the second variable.
--
-- @since 0.5.0.0
pattern Y
  :: (Eq a, Num a, KnownNat n, 2 <= n, G.Vector v (Monom n a))
  => MultiPoly v n a
pattern Y <- (isVar 1 -> True)
  where Y = var 1

pattern Y'
  :: (Eq a, Semiring a, KnownNat n, 2 <= n, G.Vector v (Monom n a))
  => MultiPoly v n a
pattern Y' <- (isVar' 1 -> True)
  where Y' = var' 1

-- | Create a polynomial equal to the third variable.
--
-- @since 0.5.0.0
pattern Z
  :: (Eq a, Num a, KnownNat n, 3 <= n, G.Vector v (Monom n a))
  => MultiPoly v n a
pattern Z <- (isVar 2 -> True)
  where Z = var 2

pattern Z'
  :: (Eq a, Semiring a, KnownNat n, 3 <= n, G.Vector v (Monom n a))
  => MultiPoly v n a
pattern Z' <- (isVar' 2 -> True)
  where Z' = var' 2

var
  :: forall v n a.
     (Eq a, Num a, KnownNat n, G.Vector v (Monom n a))
  => Finite n
  -> MultiPoly v n a
var i
  | (1 :: a) == 0 = MultiPoly G.empty
  | otherwise     = MultiPoly $ G.singleton
    (Monom (SU.generate (\j -> if i == j then 1 else 0)) 1)
{-# INLINE var #-}

var'
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (Monom n a))
  => Finite n
  -> MultiPoly v n a
var' i
  | (one :: a) == zero = MultiPoly G.empty
  | otherwise          = MultiPoly $ G.singleton
    (Monom (SU.generate (\j -> if i == j then 1 else 0)) one)
{-# INLINE var' #-}

isVar
  :: forall v n a.
     (Eq a, Num a, KnownNat n, G.Vector v (Monom n a))
  => Finite n
  -> MultiPoly v n a
  -> Bool
isVar i (MultiPoly xs)
  | (1 :: a) == 0 = G.null xs
  | otherwise     = G.length xs == 1 &&
    monomCoeff mon == 1 &&
    monomPower mon == SU.generate (\j -> if i == j then 1 else 0)
    where
      mon = G.unsafeHead xs
{-# INLINE isVar #-}

isVar'
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (Monom n a))
  => Finite n
  -> MultiPoly v n a
  -> Bool
isVar' i (MultiPoly xs)
  | (one :: a) == zero = G.null xs
  | otherwise          = G.length xs == 1 &&
    monomCoeff mon == one &&
    monomPower mon == SU.generate (\j -> if i == j then 1 else 0)
    where
      mon = G.unsafeHead xs
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
--
-- @since 0.5.0.0
segregate
  :: (G.Vector v (Monom (1 + m) a), G.Vector v (Monom m a))
  => MultiPoly v (1 + m) a
  -> VPoly (MultiPoly v m a)
segregate
  = MultiPoly
  . G.fromList
  . map (\vs -> Monom (let Monom hp _ = G.unsafeHead vs in SU.take hp) (MultiPoly $ G.map (\(Monom p c) -> Monom (SU.tail p) c) vs))
  . groupOn (\(Monom p _) -> SU.head p)
  . unMultiPoly

-- | Interpret a univariate polynomials, whose coefficients are
-- multivariate polynomials over the first /m/ variables,
-- as a multivariate polynomial over 1+/m/ variables.
--
-- @since 0.5.0.0
unsegregate
  :: (G.Vector v (Monom (1 + m) a), G.Vector v (Monom m a))
  => VPoly (MultiPoly v m a)
  -> MultiPoly v (1 + m) a
unsegregate
  = MultiPoly
  . G.concat
  . G.toList
  . G.map (\(Monom v (MultiPoly vs)) -> G.map (\(Monom p c) -> Monom (v SU.++ p) c) vs)
  . unMultiPoly
