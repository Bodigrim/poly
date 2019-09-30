-- |
-- Module:      Data.Poly.Internal.Dense
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials of one variable.
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Data.Poly.Internal.Dense
  ( Poly(..)
  , VPoly
  , UPoly
  , leading
  , dropWhileEndM
  -- * Num interface
  , toPoly
  , monomial
  , scale
  , pattern X
  , eval
  , deriv
  , integral
  -- * Semiring interface
  , toPoly'
  , monomial'
  , scale'
  , pattern X'
  , eval'
  , deriv'
#if MIN_VERSION_semirings(0,5,0)
  , integral'
#endif
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm, (^))
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.List (foldl', intersperse)
import Data.Semiring (Semiring(..), Ring())
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import GHC.Exts
#if !MIN_VERSION_semirings(0,4,0)
import Data.Semigroup
import Numeric.Natural
#endif
#if MIN_VERSION_semirings(0,5,0)
import Data.Euclidean (Field, quot)
#endif

-- | Polynomials of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use pattern 'X' for construction:
--
-- >>> (X + 1) + (X - 1) :: VPoly Integer
-- 2 * X + 0
-- >>> (X + 1) * (X - 1) :: UPoly Int
-- 1 * X^2 + 0 * X + (-1)
--
-- Polynomials are stored normalized, without leading
-- zero coefficients, so 0 * 'X' + 1 equals to 1.
--
-- 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
newtype Poly v a = Poly
  { unPoly :: v a
  -- ^ Convert 'Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  }
  deriving (Eq, NFData, Ord)

instance (Eq a, Semiring a, G.Vector v a) => IsList (Poly v a) where
  type Item (Poly v a) = a
  fromList = toPoly' . G.fromList
  fromListN = (toPoly' .) . G.fromListN
  toList = G.toList . unPoly

instance (Show a, G.Vector v a) => Show (Poly v a) where
  showsPrec d (Poly xs)
    | G.null xs
      = showString "0"
    | G.length xs == 1
      = showsPrec d (G.head xs)
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.ifoldl (\acc i c -> showCoeff i c : acc) [] xs
    where
      showCoeff 0 c = showsPrec 7 c
      showCoeff 1 c = showsPrec 7 c . showString " * X"
      showCoeff i c = showsPrec 7 c . showString " * X^" . showsPrec 7 i

-- | Polynomials backed by boxed vectors.
type VPoly = Poly V.Vector

-- | Polynomials backed by unboxed vectors.
type UPoly = Poly U.Vector

-- | Make 'Poly' from a list of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [0,0,0] :: UPoly Int
-- 0
toPoly :: (Eq a, Num a, G.Vector v a) => v a -> Poly v a
toPoly = Poly . dropWhileEnd (== 0)

toPoly' :: (Eq a, Semiring a, G.Vector v a) => v a -> Poly v a
toPoly' = Poly . dropWhileEnd (== zero)

-- | Return a leading power and coefficient of a non-zero polynomial.
--
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: UPoly Int)
-- Just (3,4)
-- >>> leading (0 :: UPoly Int)
-- Nothing
leading :: G.Vector v a => Poly v a -> Maybe (Word, a)
leading (Poly v)
  | G.null v  = Nothing
  | otherwise = Just (fromIntegral (G.length v - 1), G.last v)

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, G.Vector v a) => Num (Poly v a) where
  Poly xs + Poly ys = toPoly $ plusPoly (+) xs ys
  Poly xs - Poly ys = toPoly $ minusPoly negate (-) xs ys
  negate (Poly xs) = Poly $ G.map negate xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> Poly G.empty
    m -> Poly $ G.singleton m
  Poly xs * Poly ys = toPoly $ karatsuba xs ys
  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Semiring a, G.Vector v a) => Semiring (Poly v a) where
  zero = Poly G.empty
  one
    | (one :: a) == zero = zero
    | otherwise = Poly $ G.singleton one
  plus (Poly xs) (Poly ys) = toPoly' $ plusPoly plus xs ys
  times (Poly xs) (Poly ys) = toPoly' $ convolution zero plus times xs ys
  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}

#if MIN_VERSION_semirings(0,4,0)
  fromNatural n = if n' == zero then zero else Poly $ G.singleton n'
    where
      n' :: a
      n' = fromNatural n
  {-# INLINE fromNatural #-}
#endif

instance (Eq a, Ring a, G.Vector v a) => Ring (Poly v a) where
  negate (Poly xs) = Poly $ G.map Semiring.negate xs

dropWhileEnd
  :: G.Vector v a
  => (a -> Bool)
  -> v a
  -> v a
dropWhileEnd p xs = G.unsafeSlice 0 (go (G.length xs)) xs
  where
    go 0 = 0
    go n = if p (G.unsafeIndex xs (n - 1)) then go (n - 1) else n
{-# INLINE dropWhileEnd #-}

dropWhileEndM
  :: (PrimMonad m, G.Vector v a)
  => (a -> Bool)
  -> G.Mutable v (PrimState m) a
  -> m (G.Mutable v (PrimState m) a)
dropWhileEndM p xs = go (MG.length xs)
  where
    go 0 = pure $ MG.unsafeSlice 0 0 xs
    go n = do
      x <- MG.unsafeRead xs (n - 1)
      if p x then go (n - 1) else pure (MG.unsafeSlice 0 n xs)
{-# INLINE dropWhileEndM #-}

plusPoly
  :: G.Vector v a
  => (a -> a -> a)
  -> v a
  -> v a
  -> v a
plusPoly add xs ys = runST $ do
  let lenXs = G.length xs
      lenYs = G.length ys
      lenMn = lenXs `min` lenYs
      lenMx = lenXs `max` lenYs

  zs <- MG.unsafeNew lenMx
  forM_ [0 .. lenMn - 1] $ \i ->
    MG.unsafeWrite zs i (add (G.unsafeIndex xs i) (G.unsafeIndex ys i))
  G.unsafeCopy
    (MG.unsafeSlice lenMn (lenMx - lenMn) zs)
    (G.unsafeSlice  lenMn (lenMx - lenMn) (if lenXs <= lenYs then ys else xs))

  G.unsafeFreeze zs
{-# INLINE plusPoly #-}

minusPoly
  :: G.Vector v a
  => (a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
minusPoly neg sub xs ys = runST $ do
  let lenXs = G.length xs
      lenYs = G.length ys
      lenMn = lenXs `min` lenYs
      lenMx = lenXs `max` lenYs

  zs <- MG.unsafeNew lenMx
  forM_ [0 .. lenMn - 1] $ \i ->
    MG.unsafeWrite zs i (sub (G.unsafeIndex xs i) (G.unsafeIndex ys i))

  if lenXs < lenYs
    then forM_ [lenXs .. lenYs - 1] $ \i ->
      MG.unsafeWrite zs i (neg (G.unsafeIndex ys i))
    else G.unsafeCopy
      (MG.unsafeSlice lenYs (lenXs - lenYs) zs)
      (G.unsafeSlice  lenYs (lenXs - lenYs) xs)

  G.unsafeFreeze zs
{-# INLINE minusPoly #-}

karatsubaThreshold :: Int
karatsubaThreshold = 32

karatsuba
  :: (Eq a, Num a, G.Vector v a)
  => v a
  -> v a
  -> v a
karatsuba xs ys
  | lenXs <= karatsubaThreshold || lenYs <= karatsubaThreshold
  = convolution 0 (+) (*) xs ys
  | otherwise = runST $ do
    zs <- MG.unsafeNew lenZs
    forM_ [0 .. lenZs - 1] $ \k -> do
      let z0 = if k < G.length zs0
               then G.unsafeIndex zs0 k
               else 0
          z11 = if k - m >= 0 && k - m < G.length zs11
               then G.unsafeIndex zs11 (k - m)
               else 0
          z10 = if k - m >= 0 && k - m < G.length zs0
               then G.unsafeIndex zs0 (k - m)
               else 0
          z12 = if k - m >= 0 && k - m < G.length zs2
               then G.unsafeIndex zs2 (k - m)
               else 0
          z2 = if k - 2 * m >= 0 && k - 2 * m < G.length zs2
               then G.unsafeIndex zs2 (k - 2 * m)
               else 0
      MG.unsafeWrite zs k (z0 + (z11 - z10 - z12) + z2)
    G.unsafeFreeze zs
  where
    lenXs = G.length xs
    lenYs = G.length ys
    lenZs = lenXs + lenYs - 1

    m    = ((lenXs `min` lenYs) + 1) `shiftR` 1

    xs0  = G.slice 0 m xs
    xs1  = G.slice m (lenXs - m) xs
    ys0  = G.slice 0 m ys
    ys1  = G.slice m (lenYs - m) ys

    xs01 = plusPoly (+) xs0 xs1
    ys01 = plusPoly (+) ys0 ys1
    zs0  = karatsuba xs0 ys0
    zs2  = karatsuba xs1 ys1
    zs11 = karatsuba xs01 ys01
{-# INLINE karatsuba #-}

convolution
  :: G.Vector v a
  => a
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
convolution zer add mul xs ys
  | lenXs == 0 || lenYs == 0 = G.empty
  | otherwise = G.generate lenZs $ \k -> foldl'
    (\acc i -> acc `add` mul (G.unsafeIndex xs i) (G.unsafeIndex ys (k - i)))
    zer
    [max (k - lenYs + 1) 0 .. min k (lenXs - 1)]
  where
    lenXs = G.length xs
    lenYs = G.length ys
    lenZs = lenXs + lenYs - 1
{-# INLINE convolution #-}

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Num a, G.Vector v a) => Word -> a -> Poly v a
monomial _ 0 = Poly G.empty
monomial p c = Poly $ G.generate (fromIntegral p + 1) (\i -> if i == fromIntegral p then c else 0)
{-# INLINE monomial #-}

monomial' :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a
monomial' p c
  | c == zero = Poly G.empty
  | otherwise = Poly $ G.generate (fromIntegral p + 1) (\i -> if i == fromIntegral p then c else zero)
{-# INLINE monomial' #-}

scaleInternal
  :: (Eq a, G.Vector v a)
  => a
  -> (a -> a -> a)
  -> Word
  -> a
  -> v a
  -> v a
scaleInternal zer mul yp yc xs = runST $ do
  let lenXs = G.length xs
  zs <- MG.unsafeNew (fromIntegral yp + lenXs)
  forM_ [0 .. fromIntegral yp - 1] $ \k ->
    MG.unsafeWrite zs k zer
  forM_ [0 .. lenXs - 1] $ \k ->
    MG.unsafeWrite zs (fromIntegral yp + k) (mul yc $ G.unsafeIndex xs k)
  G.unsafeFreeze zs
{-# INLINE scaleInternal #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 0 * X^3 + 3 * X^2 + 0 * X + 0
scale :: (Eq a, Num a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
scale yp yc (Poly xs) = toPoly $ scaleInternal 0 (*) yp yc xs

scale' :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
scale' yp yc (Poly xs) = toPoly' $ scaleInternal zero times yp yc xs

data StrictPair a b = !a :*: !b

infixr 1 :*:

fst' :: StrictPair a b -> a
fst' (a :*: _) = a

-- | Evaluate at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Num a, G.Vector v a) => Poly v a -> a -> a
eval (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> acc + cn * xn :*: x * xn) (0 :*: 1) cs
{-# INLINE eval #-}

eval' :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval' (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> acc `plus` cn `times` xn :*: x `times` xn) (zero :*: one) cs
{-# INLINE eval' #-}

-- | Take a derivative.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 0 * X + 3
deriv :: (Eq a, Num a, G.Vector v a) => Poly v a -> Poly v a
deriv (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly $ G.imap (\i x -> fromIntegral (i + 1) * x) $ G.tail xs
{-# INLINE deriv #-}

deriv' :: (Eq a, Semiring a, G.Vector v a) => Poly v a -> Poly v a
deriv' (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly' $ G.imap (\i x -> fromNatural (fromIntegral (i + 1)) `times` x) $ G.tail xs
{-# INLINE deriv' #-}

#if !MIN_VERSION_semirings(0,4,0)
fromNatural :: Semiring a => Natural -> a
fromNatural 0 = zero
fromNatural n = getAdd' (stimes n (Add' one))

newtype Add' a = Add' { getAdd' :: a }

instance Semiring a => Semigroup (Add' a) where
  Add' a <> Add' b = Add' (a `plus` b)
#endif

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
integral :: (Eq a, Fractional a, G.Vector v a) => Poly v a -> Poly v a
integral (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly $ runST $ do
    zs <- MG.unsafeNew (lenXs + 1)
    MG.unsafeWrite zs 0 0
    forM_ [0 .. lenXs - 1] $ \i ->
      MG.unsafeWrite zs (i + 1) (G.unsafeIndex xs i * recip (fromIntegral i + 1))
    G.unsafeFreeze zs
    where
      lenXs = G.length xs
{-# INLINE integral #-}

#if MIN_VERSION_semirings(0,5,0)
integral' :: (Eq a, Field a, G.Vector v a) => Poly v a -> Poly v a
integral' (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly' $ runST $ do
    zs <- MG.unsafeNew (lenXs + 1)
    MG.unsafeWrite zs zero zero
    forM_ [0 .. lenXs - 1] $ \i ->
      MG.unsafeWrite zs (i + 1) (G.unsafeIndex xs i `quot` Semiring.fromIntegral (i + 1))
    G.unsafeFreeze zs
    where
      lenXs = G.length xs
{-# INLINE integral' #-}
#endif

-- | Create an identity polynomial.
pattern X :: (Eq a, Num a, G.Vector v a, Eq (v a)) => Poly v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v a, Eq (v a)) => Poly v a
var
  | (1 :: a) == 0 = Poly G.empty
  | otherwise     = Poly $ G.fromList [0, 1]
{-# INLINE var #-}

-- | Create an identity polynomial.
pattern X' :: (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
pattern X' <- ((==) var' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
var'
  | (one :: a) == zero = Poly G.empty
  | otherwise          = Poly $ G.fromList [zero, one]
{-# INLINE var' #-}
