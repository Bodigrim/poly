-- |
-- Module:      Data.Poly.Internal.Dense.Laurent
-- Copyright:   (c) 2020 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Poly.Internal.Dense.Laurent
  ( Laurent
  , VLaurent
  , ULaurent
  , unLaurent
  , toLaurent
  , leading
  , monomial
  , scale
  , pattern X
  , (^-)
  , eval
  , subst
  , deriv
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm)
import Control.Arrow (first)
import Control.DeepSeq (NFData(..))
import Control.Exception
import Data.Euclidean (GcdDomain(..), Euclidean(..), Field)
import Data.Kind
import Data.List (intersperse)
import Data.Semiring (Semiring(..), Ring())
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

import Data.Poly.Internal.Dense (Poly(..))
import qualified Data.Poly.Internal.Dense as Dense
import Data.Poly.Internal.Dense.Field ()
import Data.Poly.Internal.Dense.GcdDomain ()

-- | <https://en.wikipedia.org/wiki/Laurent_polynomial Laurent polynomials>
-- of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use the pattern 'X' and the '^-' operator for construction:
--
-- >>> (X + 1) + (X^-1 - 1) :: VLaurent Integer
-- 1 * X + 0 + 1 * X^-1
-- >>> (X + 1) * (1 - X^-1) :: ULaurent Int
-- 1 * X + 0 + (-1) * X^-1
--
-- Polynomials are stored normalized, without leading
-- and trailing
-- zero coefficients, so 0 * X + 1 + 0 * X^-1 equals to 1.
--
-- The 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
data Laurent (v :: Type -> Type) (a :: Type) = Laurent !Int !(Poly v a)
  deriving (Eq, Ord)

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
unLaurent (Laurent off poly) = (off, poly)

-- | Construct 'Laurent' polynomial from an offset and a regular polynomial.
-- One can imagine it as 'Data.Poly.Semiring.scale', but allowing negative offsets.
--
-- >>> toLaurent 2 (2 * Data.Poly.X + 1) :: ULaurent Int
-- 2 * X^3 + 1 * X^2
-- >>> toLaurent (-2) (2 * Data.Poly.X + 1) :: ULaurent Int
-- 2 * X^-1 + 1 * X^-2
toLaurent
  :: (Eq a, Semiring a, G.Vector v a)
  => Int
  -> Poly v a
  -> Laurent v a
toLaurent off (Poly xs) = go 0
  where
    go k
      | k >= G.length xs
      = Laurent 0 zero
      | G.unsafeIndex xs k == zero
      = go (k + 1)
      | otherwise
      = Laurent (off + k) (Poly (G.unsafeDrop k xs))
{-# INLINE toLaurent #-}

toLaurentNum
  :: (Eq a, Num a, G.Vector v a)
  => Int
  -> Poly v a
  -> Laurent v a
toLaurentNum off (Poly xs) = go 0
  where
    go k
      | k >= G.length xs
      = Laurent 0 0
      | G.unsafeIndex xs k == 0
      = go (k + 1)
      | otherwise
      = Laurent (off + k) (Poly (G.unsafeDrop k xs))
{-# INLINE toLaurentNum #-}

instance NFData (v a) => NFData (Laurent v a) where
  rnf (Laurent off poly) = rnf off `seq` rnf poly

instance (Show a, G.Vector v a) => Show (Laurent v a) where
  showsPrec d (Laurent off poly)
    | G.null (unPoly poly)
      = showString "0"
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.ifoldl (\acc i c -> showCoeff (i + off) c : acc) []
      $ unPoly poly
    where
      -- Negative powers should be displayed without surrounding brackets
      showCoeff 0 c = showsPrec 7 c
      showCoeff 1 c = showsPrec 7 c . showString " * X"
      showCoeff i c = showsPrec 7 c . showString (" * X^" ++ show i)

-- | Laurent polynomials backed by boxed vectors.
type VLaurent = Laurent V.Vector

-- | Laurent polynomials backed by unboxed vectors.
type ULaurent = Laurent U.Vector

-- | Return the leading power and coefficient of a non-zero polynomial.
--
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: ULaurent Int)
-- Just (3,4)
-- >>> leading (0 :: ULaurent Int)
-- Nothing
leading :: G.Vector v a => Laurent v a -> Maybe (Int, a)
leading (Laurent off poly) = first ((+ off) . fromIntegral) <$> Dense.leading poly

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, G.Vector v a) => Num (Laurent v a) where
  Laurent off1 poly1 * Laurent off2 poly2 = toLaurentNum (off1 + off2) (poly1 * poly2)
  Laurent off1 poly1 + Laurent off2 poly2 = case off1 `compare` off2 of
    LT -> toLaurentNum off1 (poly1 + Dense.scale (fromIntegral $ off2 - off1) 1 poly2)
    EQ -> toLaurentNum off1 (poly1 + poly2)
    GT -> toLaurentNum off2 (Dense.scale (fromIntegral $ off1 - off2) 1 poly1 + poly2)
  Laurent off1 poly1 - Laurent off2 poly2 = case off1 `compare` off2 of
    LT -> toLaurentNum off1 (poly1 - Dense.scale (fromIntegral $ off2 - off1) 1 poly2)
    EQ -> toLaurentNum off1 (poly1 - poly2)
    GT -> toLaurentNum off2 (Dense.scale (fromIntegral $ off1 - off2) 1 poly1 - poly2)
  negate (Laurent off poly) = Laurent off (negate poly)
  abs = id
  signum = const 1
  fromInteger n = Laurent 0 (fromInteger n)
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Semiring a, G.Vector v a) => Semiring (Laurent v a) where
  zero = Laurent 0 zero
  one  = Laurent 0 one
  Laurent off1 poly1 `times` Laurent off2 poly2 =
    toLaurent (off1 + off2) (poly1 `times` poly2)
  Laurent off1 poly1 `plus` Laurent off2 poly2 = case off1 `compare` off2 of
    LT -> toLaurent off1 (poly1 `plus` Dense.scale' (fromIntegral $ off2 - off1) one poly2)
    EQ -> toLaurent off1 (poly1 `plus` poly2)
    GT -> toLaurent off2 (Dense.scale' (fromIntegral $ off1 - off2) one poly1 `plus` poly2)
  fromNatural n = Laurent 0 (fromNatural n)
  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}
  {-# INLINE fromNatural #-}

instance (Eq a, Ring a, G.Vector v a) => Ring (Laurent v a) where
  negate (Laurent off poly) = Laurent off (Semiring.negate poly)

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Semiring a, G.Vector v a) => Int -> a -> Laurent v a
monomial p c
  | c == zero = Laurent 0 zero
  | otherwise = Laurent p (Dense.monomial' 0 c)
{-# INLINE monomial #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^-2 + 1) :: ULaurent Int
-- 3 * X^2 + 0 * X + 3
scale :: (Eq a, Semiring a, G.Vector v a) => Int -> a -> Laurent v a -> Laurent v a
scale yp yc (Laurent off poly) = toLaurent (off + yp) (Dense.scale' 0 yc poly)

-- | Evaluate the polynomial at a given point.
--
-- >>> eval (X^-2 + 1 :: ULaurent Double) 2
-- 1.25
eval :: (Field a, G.Vector v a) => Laurent v a -> a -> a
eval (Laurent off poly) x = Dense.eval' poly x `times`
  (if off >= 0 then x Semiring.^ off else quot one x Semiring.^ (- off))
{-# INLINE eval #-}

-- | Substitute another polynomial instead of 'Data.Poly.X'.
--
-- >>> import Data.Poly (UPoly)
-- >>> subst (Data.Poly.X^2 + 1 :: UPoly Int) (X^-1 + 1 :: ULaurent Int)
-- 2 + 2 * X^-1 + 1 * X^-2
subst :: (Eq a, Semiring a, G.Vector v a, G.Vector w a) => Poly v a -> Laurent w a -> Laurent w a
subst = Dense.substitute' (scale 0)
{-# INLINE subst #-}

-- | Take the derivative of the polynomial.
--
-- >>> deriv (X^-1 + 3 * X) :: ULaurent Int
-- 3 + 0 * X^-1 + (-1) * X^-2
deriv :: (Eq a, Ring a, G.Vector v a) => Laurent v a -> Laurent v a
deriv (Laurent off (Poly xs)) =
  toLaurent (off - 1) $ Dense.toPoly' $ G.imap (times . Semiring.fromIntegral . (+ off)) xs
{-# INLINE deriv #-}

-- | Create an identity polynomial.
pattern X :: (Eq a, Semiring a, G.Vector v a) => Laurent v a
pattern X <- (isVar -> True)
  where X = var

var :: forall a v. (Eq a, Semiring a, G.Vector v a) => Laurent v a
var
  | (one :: a) == zero = Laurent 0 zero
  | otherwise          = Laurent 1 one
{-# INLINE var #-}

isVar :: forall v a. (Eq a, Semiring a, G.Vector v a) => Laurent v a -> Bool
isVar (Laurent off (Poly xs))
  | (one :: a) == zero = off == 0 && G.null xs
  | otherwise          = off == 1 && G.length xs == 1 && G.unsafeHead xs == one
{-# INLINE isVar #-}

-- | This operator can be applied only to monomials with unit coefficients,
-- but is instrumental to express Laurent polynomials
-- in mathematical fashion:
--
-- >>> X + 2 + 3 * (X^2)^-1 :: ULaurent Int
-- 1 * X + 2 + 0 * X^-1 + 3 * X^-2
(^-)
  :: (Eq a, Num a, G.Vector v a)
  => Laurent v a
  -> Int
  -> Laurent v a
Laurent off (Poly xs) ^- n
  | G.length xs == 1, G.unsafeHead xs == 1
  = Laurent (off * (-n)) (Poly xs)
  | otherwise
  = throw $ PatternMatchFail "(^-) can be applied only to a monom with unit coefficient"

instance (Eq a, Ring a, GcdDomain a, G.Vector v a) => GcdDomain (Laurent v a) where
  divide (Laurent off1 poly1) (Laurent off2 poly2) =
    toLaurent (off1 - off2) <$> divide poly1 poly2
  {-# INLINE divide #-}

  gcd (Laurent _ poly1) (Laurent _ poly2) =
    toLaurent 0 (gcd poly1 poly2)
  {-# INLINE gcd #-}

  lcm (Laurent _ poly1) (Laurent _ poly2) =
    toLaurent 0 (lcm poly1 poly2)
  {-# INLINE lcm #-}

  coprime (Laurent _ poly1) (Laurent _ poly2) =
    coprime poly1 poly2
  {-# INLINE coprime #-}
