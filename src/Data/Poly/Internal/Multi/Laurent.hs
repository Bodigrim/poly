-- |
-- Module:      Data.Poly.Internal.Multi.Laurent
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Sparse multivariate
-- <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>.
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE ViewPatterns               #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints      #-}
#endif

module Data.Poly.Internal.Multi.Laurent
  ( MultiLaurent
  , VMultiLaurent
  , UMultiLaurent
  , unMultiLaurent
  , toMultiLaurent
  , leading
  , monomial
  , scale
  , pattern X
  , pattern Y
  , pattern Z
  , (^-)
  , eval
  , subst
  , deriv
  -- * Univariate polynomials
  , Laurent
  , VLaurent
  , ULaurent
  , unLaurent
  , toLaurent
  -- * Conversions
  , segregate
  , unsegregate
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm)
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Exception
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Finite
import Data.Kind
import Data.List (intersperse, foldl1')
import Data.Semiring (Semiring(..), Ring())
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.Exts

import Data.Poly.Internal.Multi.Core (derivPoly)
import Data.Poly.Internal.Multi (Poly, MultiPoly(..))
import qualified Data.Poly.Internal.Multi as Multi
import Data.Poly.Internal.Multi.Field ()
import Data.Poly.Internal.Multi.GcdDomain ()

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (KnownNat, Nat, type (+), type (<=))
#else
import GHC.TypeLits (KnownNat, Nat, type (+), type (<=))
#endif

-- | Sparse
-- <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>
-- of @n@ variables with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use patterns 'X', 'Y', 'Z' and operator '^-' for construction:
--
-- >>> (X + 1) + (Y^-1 - 1) :: VMultiLaurent 2 Integer
-- 1 * X + 1 * Y^-1
-- >>> (X + 1) * (Z - X^-1) :: UMultiLaurent 3 Int
-- 1 * X * Z + 1 * Z + (-1) + (-1) * X^-1
--
-- Polynomials are stored normalized, without
-- zero coefficients, so 0 * X + 1 + 0 * X^-1 equals to 1.
--
-- 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
data MultiLaurent (v :: Type -> Type) (n :: Nat) (a :: Type) =
  MultiLaurent !(SU.Vector n Int) !(MultiPoly v n a)

deriving instance Eq  (v (SU.Vector n Word, a)) => Eq  (MultiLaurent v n a)
deriving instance Ord (v (SU.Vector n Word, a)) => Ord (MultiLaurent v n a)

-- | Multivariate Laurent polynomials backed by boxed vectors.
type VMultiLaurent (n :: Nat) (a :: Type) = MultiLaurent V.Vector n a

-- | Multivariate Laurent polynomials backed by unboxed vectors.
type UMultiLaurent (n :: Nat) (a :: Type) = MultiLaurent U.Vector n a

-- | <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>
-- of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use pattern 'X' and operator '^-' for construction:
--
-- >>> (X + 1) + (X^-1 - 1) :: VLaurent Integer
-- 1 * X + 1 * X^-1
-- >>> (X + 1) * (1 - X^-1) :: ULaurent Int
-- 1 * X + (-1) * X^-1
--
-- Polynomials are stored normalized, without
-- zero coefficients, so 0 * X + 1 + 0 * X^-1 equals to 1.
--
-- 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
type Laurent (v :: Type -> Type) (a :: Type) = MultiLaurent v 1 a

-- | Laurent polynomials backed by boxed vectors.
type VLaurent (a :: Type) = Laurent V.Vector a

-- | Laurent polynomials backed by unboxed vectors.
type ULaurent (a :: Type) = Laurent U.Vector a

instance (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Int, a), G.Vector v (SU.Vector n Word, a)) => IsList (MultiLaurent v n a) where
  type Item (MultiLaurent v n a) = (SU.Vector n Int, a)

  fromList [] = MultiLaurent 0 zero
  fromList xs = toMultiLaurent minPow (fromList ys)
    where
      minPow = foldl1' (SU.zipWith min) (map fst xs)
      ys = map (first (SU.map fromIntegral . subtract minPow)) xs

  toList (MultiLaurent off (MultiPoly poly)) =
    map (first ((+ off) . SU.map fromIntegral)) $ G.toList poly

-- | Deconstruct a 'MultiLaurent' polynomial into an offset (largest possible)
-- and a regular polynomial.
--
-- >>> unMultiLaurent (2 * X + 1 :: UMultiLaurent 2 Int)
-- (Vector [0,0],2 * X + 1)
-- >>> unMultiLaurent (1 + 2 * X^-1 :: UMultiLaurent 2 Int)
-- (Vector [-1,0],1 * X + 2)
-- >>> unMultiLaurent (2 * X^2 + X :: UMultiLaurent 2 Int)
-- (Vector [1,0],2 * X + 1)
-- >>> unMultiLaurent (0 :: UMultiLaurent 2 Int)
-- (Vector [0,0],0)
unMultiLaurent :: MultiLaurent v n a -> (SU.Vector n Int, MultiPoly v n a)
unMultiLaurent (MultiLaurent off poly) = (off, poly)

-- | Deconstruct a 'Laurent' polynomial into an offset (largest possible)
-- and a regular polynomial.
--
-- >>> unLaurent (2 * X + 1 :: ULaurent Int)
-- (0,2 * X + 1)
-- >>> unLaurent (1 + 2 * X^-1 :: ULaurent Int)
-- (-1,1 * X + 2)
-- >>> unLaurent (2 * X^2 + X :: ULaurent Int)
-- (1,2 * X + 1)
-- >>> unLaurent (0 :: ULaurent Int)
-- (0,0)
unLaurent :: Laurent v a -> (Int, Poly v a)
unLaurent = first SU.head . unMultiLaurent

-- | Construct 'MultiLaurent' polynomial from an offset and a regular polynomial.
-- One can imagine it as 'Data.Poly.Multi.Semiring.scale', but allowing negative offsets.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> toMultiLaurent (fromTuple (2, 0)) (2 * Data.Poly.Multi.X + 1) :: UMultiLaurent 2 Int
-- 2 * X^3 + 1 * X^2
-- >>> toMultiLaurent (fromTuple (0, -2)) (2 * Data.Poly.Multi.X + 1) :: UMultiLaurent 2 Int
-- 2 * X * Y^-2 + 1 * Y^-2
toMultiLaurent
  :: (KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Int
  -> MultiPoly v n a
  -> MultiLaurent v n a
toMultiLaurent off (MultiPoly xs)
  | G.null xs = MultiLaurent 0 (MultiPoly G.empty)
  | otherwise = MultiLaurent (SU.zipWith (\o m -> o + fromIntegral m) off minPow) (MultiPoly ys)
    where
      minPow = G.foldl'(\acc (x, _) -> SU.zipWith min acc x) (SU.replicate maxBound) xs
      ys
        | SU.all (== 0) minPow = xs
        | otherwise = G.map (first (SU.zipWith subtract minPow)) xs
{-# INLINE toMultiLaurent #-}

-- | Construct 'Laurent' polynomial from an offset and a regular polynomial.
-- One can imagine it as 'Data.Poly.Sparse.Semiring.scale', but allowing negative offsets.
--
-- >>> toLaurent 2 (2 * Data.Poly.Sparse.X + 1) :: ULaurent Int
-- 2 * X^3 + 1 * X^2
-- >>> toLaurent (-2) (2 * Data.Poly.Sparse.X + 1) :: ULaurent Int
-- 2 * X^-1 + 1 * X^-2
toLaurent
  :: G.Vector v (SU.Vector 1 Word, a)
  => Int
  -> Poly v a
  -> Laurent v a
toLaurent = toMultiLaurent . SU.singleton

instance NFData (v (SU.Vector n Word, a)) => NFData (MultiLaurent v n a) where
  rnf (MultiLaurent off poly) = rnf off `seq` rnf poly

instance (Show a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Show (MultiLaurent v n a) where
  showsPrec d (MultiLaurent off (MultiPoly xs))
    | G.null xs
      = showString "0"
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.foldl (\acc (is, c) -> showCoeff (SU.map fromIntegral is + off) c : acc) [] xs
    where
      showCoeff is c
        = showsPrec 7 c . foldl (.) id
        ( map ((showString " * " .) . uncurry showPower)
        $ filter ((/= 0) . fst)
        $ zip (SU.toList is) (finites :: [Finite n]))

      -- Negative powers should be displayed without surrounding brackets
      showPower :: Int -> Finite n -> String -> String
      showPower 1 n = showString (showVar n)
      showPower i n = showString (showVar n) . showString ("^" ++ show i)

      showVar :: Finite n -> String
      showVar = \case
        0 -> "X"
        1 -> "Y"
        2 -> "Z"
        k -> "X" ++ show k

-- | Return a leading power and coefficient of a non-zero polynomial.
--
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: ULaurent Int)
-- Just (3,4)
-- >>> leading (0 :: ULaurent Int)
-- Nothing
leading :: G.Vector v (SU.Vector 1 Word, a) => Laurent v a -> Maybe (Int, a)
leading (MultiLaurent off poly) = first ((+ SU.head off) . fromIntegral) <$> Multi.leading poly

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Num (MultiLaurent v n a) where
  MultiLaurent off1 poly1 * MultiLaurent off2 poly2 = toMultiLaurent (off1 + off2) (poly1 * poly2)
  MultiLaurent off1 poly1 + MultiLaurent off2 poly2 = toMultiLaurent off (poly1' + poly2')
    where
      off    = SU.zipWith min off1 off2
      poly1' = Multi.scale (SU.zipWith (\x y -> fromIntegral (x - y)) off1 off) 1 poly1
      poly2' = Multi.scale (SU.zipWith (\x y -> fromIntegral (x - y)) off2 off) 1 poly2
  MultiLaurent off1 poly1 - MultiLaurent off2 poly2 = toMultiLaurent off (poly1' - poly2')
    where
      off    = SU.zipWith min off1 off2
      poly1' = Multi.scale (SU.zipWith (\x y -> fromIntegral (x - y)) off1 off) 1 poly1
      poly2' = Multi.scale (SU.zipWith (\x y -> fromIntegral (x - y)) off2 off) 1 poly2
  negate (MultiLaurent off poly) = MultiLaurent off (negate poly)
  abs = id
  signum = const 1
  fromInteger n = MultiLaurent 0 (fromInteger n)
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Semiring (MultiLaurent v n a) where
  zero = MultiLaurent 0 zero
  one  = MultiLaurent 0 one
  MultiLaurent off1 poly1 `times` MultiLaurent off2 poly2 =
    toMultiLaurent (off1 + off2) (poly1 `times` poly2)
  MultiLaurent off1 poly1 `plus` MultiLaurent off2 poly2 = toMultiLaurent off (poly1' `plus` poly2')
    where
      off    = SU.zipWith min off1 off2
      poly1' = Multi.scale' (SU.zipWith (\x y -> fromIntegral (x - y)) off1 off) one poly1
      poly2' = Multi.scale' (SU.zipWith (\x y -> fromIntegral (x - y)) off2 off) one poly2
  fromNatural n = MultiLaurent 0 (fromNatural n)
  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}
  {-# INLINE fromNatural #-}

instance (Eq a, Ring a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Ring (MultiLaurent v n a) where
  negate (MultiLaurent off poly) = MultiLaurent off (Semiring.negate poly)

-- | Create a monomial from a power and a coefficient.
monomial
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Int
  -> a
  -> MultiLaurent v n a
monomial p c
  | c == zero = MultiLaurent 0 zero
  | otherwise = MultiLaurent p (Multi.monomial' 0 c)
{-# INLINE monomial #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> scale (fromTuple (1, 1)) 3 (X^-2 + Y) :: UMultiLaurent 2 Int
-- 3 * X * Y^2 + 3 * X^-1 * Y
scale
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => SU.Vector n Int
  -> a
  -> MultiLaurent v n a
  -> MultiLaurent v n a
scale yp yc (MultiLaurent off poly) = toMultiLaurent (off + yp) (Multi.scale' 0 yc poly)

-- | Evaluate at a given point.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> eval (X^2 + Y^-1 :: UMultiLaurent 2 Double) (fromTuple (3, 4) :: Data.Vector.Sized.Vector 2 Double)
-- 9.25
eval
  :: (Field a, G.Vector v (SU.Vector n Word, a), G.Vector u a)
  => MultiLaurent v n a
  -> SG.Vector u n a
  -> a
eval (MultiLaurent off poly) xs = Multi.eval' poly xs `times`
  SU.ifoldl' (\acc i o -> acc `times` (let x = SG.index xs i in if o >= 0 then x Semiring.^ o else quot one x Semiring.^ (- o))) one off
{-# INLINE eval #-}

-- | Substitute another polynomial instead of 'Data.Poly.Multi.X'.
--
-- >>> :set -XDataKinds
-- >>> import Data.Vector.Generic.Sized (fromTuple)
-- >>> import Data.Poly.Multi (UMultiPoly)
-- >>> subst (Data.Poly.Multi.X * Data.Poly.Multi.Y :: UMultiPoly 2 Int) (fromTuple (X + Y^-1, Y + X^-1 :: UMultiLaurent 2 Int))
-- 1 * X * Y + 2 + 1 * X^-1 * Y^-1
subst
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a), G.Vector w (SU.Vector n Word, a))
  => MultiPoly v n a
  -> SV.Vector n (MultiLaurent w n a)
  -> MultiLaurent w n a
subst = Multi.substitute' (scale 0)
{-# INLINE subst #-}

-- | Take a derivative with respect to the /i/-th variable.
--
-- >>> :set -XDataKinds
-- >>> deriv 0 (X^3 + 3 * Y) :: UMultiLaurent 2 Int
-- 3 * X^2
-- >>> deriv 1 (X^3 + 3 * Y) :: UMultiLaurent 2 Int
-- 3
deriv
  :: (Eq a, Ring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiLaurent v n a
  -> MultiLaurent v n a
deriv i (MultiLaurent off (MultiPoly xs)) =
  toMultiLaurent (off SU.// [(i, off `SU.index` i - 1)]) $ MultiPoly $ derivPoly
    (/= zero)
    id
    (\ps c -> Semiring.fromIntegral (fromIntegral (ps `SU.index` i) + off `SU.index` i) `times` c)
    xs
{-# INLINE deriv #-}

-- | Create a polynomial equal to the first variable.
pattern X
  :: (Eq a, Semiring a, KnownNat n, 1 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiLaurent v n a
pattern X <- (isVar 0 -> True)
  where X = var 0

-- | Create a polynomial equal to the second variable.
pattern Y
  :: (Eq a, Semiring a, KnownNat n, 2 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiLaurent v n a
pattern Y <- (isVar 1 -> True)
  where Y = var 1

-- | Create a polynomial equal to the third variable.
pattern Z
  :: (Eq a, Semiring a, KnownNat n, 3 <= n, G.Vector v (SU.Vector n Word, a))
  => MultiLaurent v n a
pattern Z <- (isVar 2 -> True)
  where Z = var 2

var
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiLaurent v n a
var i
  | (one :: a) == zero = MultiLaurent 0 zero
  | otherwise          = MultiLaurent
      (SU.generate (\j -> if i == j then 1 else 0)) one
{-# INLINE var #-}

isVar
  :: forall v n a.
     (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => Finite n
  -> MultiLaurent v n a
  -> Bool
isVar i (MultiLaurent off (MultiPoly xs))
  | (one :: a) == zero
  = off == 0 && G.null xs
  | otherwise
  = off == SU.generate (\j -> if i == j then 1 else 0)
  && G.length xs == 1 && G.unsafeHead xs == (0, one)
{-# INLINE isVar #-}

-- | This operator can be applied only to monomials with unit coefficients,
-- but is still instrumental to express Laurent polynomials
-- in mathematical fashion:
--
-- >>> 3 * X^-1 + 2 * (Y^2)^-2 :: UMultiLaurent 2 Int
-- 2 * Y^-4 + 3 * X^-1
(^-)
  :: (Eq a, Semiring a, KnownNat n, G.Vector v (SU.Vector n Word, a))
  => MultiLaurent v n a
  -> Int
  -> MultiLaurent v n a
MultiLaurent off (MultiPoly xs) ^- n
  | G.length xs == 1, G.unsafeHead xs == (0, one)
  = MultiLaurent (SU.map (* (-n)) off) (MultiPoly xs)
  | otherwise
  = throw $ PatternMatchFail "(^-) can be applied only to a monom with unit coefficient"

instance {-# OVERLAPPING #-} (Eq a, Ring a, GcdDomain a, G.Vector v (SU.Vector 1 Word, a)) => GcdDomain (Laurent v a) where
  divide (MultiLaurent off1 poly1) (MultiLaurent off2 poly2) =
    toMultiLaurent (off1 - off2) <$> divide poly1 poly2
  {-# INLINE divide #-}

  gcd (MultiLaurent _ poly1) (MultiLaurent _ poly2) =
    toMultiLaurent 0 (gcd poly1 poly2)
  {-# INLINE gcd #-}

  lcm (MultiLaurent _ poly1) (MultiLaurent _ poly2) =
    toMultiLaurent 0 (lcm poly1 poly2)
  {-# INLINE lcm #-}

  coprime (MultiLaurent _ poly1) (MultiLaurent _ poly2) =
    coprime poly1 poly2
  {-# INLINE coprime #-}

#if __GLASGOW_HASKELL__ >= 806
instance (Eq a, Ring a, GcdDomain a, KnownNat n, forall m. KnownNat m => G.Vector v (SU.Vector m Word, a), forall m. KnownNat m => Eq (v (SU.Vector m Word, a))) => GcdDomain (MultiLaurent v n a) where
#else
instance (Eq a, Ring a, GcdDomain a, KnownNat n, v ~ V.Vector) => GcdDomain (MultiLaurent v n a) where
#endif
  divide (MultiLaurent off1 poly1) (MultiLaurent off2 poly2) =
    toMultiLaurent (off1 - off2) <$> divide poly1 poly2
  {-# INLINE divide #-}

  gcd (MultiLaurent _ poly1) (MultiLaurent _ poly2) =
    toMultiLaurent 0 (gcd poly1 poly2)
  {-# INLINE gcd #-}

  lcm (MultiLaurent _ poly1) (MultiLaurent _ poly2) =
    toMultiLaurent 0 (lcm poly1 poly2)
  {-# INLINE lcm #-}

  coprime (MultiLaurent _ poly1) (MultiLaurent _ poly2) =
    coprime poly1 poly2
  {-# INLINE coprime #-}

-------------------------------------------------------------------------------

-- | Interpret a multivariate Laurent polynomial over 1+/m/ variables
-- as a univariate Laurent polynomial, whose coefficients are
-- multivariate Laurent polynomials over the last /m/ variables.
segregate
  :: (KnownNat m, G.Vector v (SU.Vector (1 + m) Word, a), G.Vector v (SU.Vector m Word, a))
  => MultiLaurent v (1 + m) a
  -> VLaurent (MultiLaurent v m a)
segregate (MultiLaurent off poly)
  = toMultiLaurent (SU.take off)
  $ MultiPoly
  $ G.map (fmap (toMultiLaurent (SU.tail off)))
  $ Multi.unMultiPoly
  $ Multi.segregate poly

-- | Interpret a univariate Laurent polynomials, whose coefficients are
-- multivariate Laurent polynomials over the first /m/ variables,
-- as a multivariate polynomial over 1+/m/ variables.
unsegregate
  :: forall v m a.
     (KnownNat m, KnownNat (1 + m), G.Vector v (SU.Vector (1 + m) Word, a), G.Vector v (SU.Vector m Word, a))
  => VLaurent (MultiLaurent v m a)
  -> MultiLaurent v (1 + m) a
unsegregate (MultiLaurent off poly)
  | G.null (unMultiPoly poly)
  = MultiLaurent 0 (MultiPoly G.empty)
  | otherwise
  = toMultiLaurent (off SU.++ offs) (MultiPoly (G.concat (G.toList ys)))
  where
    xs :: V.Vector (SU.Vector 1 Word, (SU.Vector m Int, MultiPoly v m a))
    xs = G.map (fmap unMultiLaurent) $ Multi.unMultiPoly poly
    offs :: SU.Vector m Int
    offs = G.foldl' (\acc (_, (v, _)) -> SU.zipWith min acc v) (SU.replicate maxBound) xs
    ys :: V.Vector (v (SU.Vector (1 + m) Word, a))
    ys = G.map (\(v, (vs, p)) -> G.map (first ((v SU.++) . SU.zipWith3 (\a b c -> c + fromIntegral (b - a)) offs vs)) (unMultiPoly p)) xs
