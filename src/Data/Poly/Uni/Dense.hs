-- |
-- Module:      Data.Poly.Uni.Dense
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials of one variable.
--

{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Poly.Uni.Dense
  ( Poly
  , VPoly
  , UPoly
  , unPoly
  -- * Num interface
  , toPoly
  , constant
  , pattern X
  , eval
  , deriv
  , integral
  -- * Semiring interface
  , toPoly'
  , constant'
  , pattern X'
  , eval'
  , deriv'
  ) where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.List (foldl')
import Data.Semigroup (stimes)
import Data.Semiring (Semiring(..), Add(..))
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U

-- | Polynomials of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- >>> :set -XOverloadedLists
-- >>> -- (1 + x) * (-1 + x) = (-1 + x^2)
-- >>> toPoly [1,1] * toPoly [-1,1] :: VPoly Integer
-- Poly {unPoly = [-1,0,1]}
--
-- >>> :set -XOverloadedLists
-- >>> -- (1 + x) + (1 - x) = 2
-- >>> toPoly [1,1] + toPoly [1,-1] :: UPoly Int
-- Poly {unPoly = [2]}
newtype Poly v a = Poly
  { unPoly :: v a
  -- ^ Convert 'Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  }
  deriving (Eq, Ord, Show, Functor)

-- | Polynomials backed by boxed vectors.
type VPoly = Poly V.Vector

-- | Polynomials backed by unboxed vectors.
type UPoly = Poly U.Vector

-- | Make 'Poly' from a list of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3] :: VPoly Integer
-- Poly {unPoly = [1,2,3]}
-- >>> toPoly [0,0,0] :: UPoly Int
-- Poly {unPoly = []}
toPoly :: (Eq a, Num a, G.Vector v a) => v a -> Poly v a
toPoly = Poly . dropWhileEnd (== 0)

-- | Make 'Poly' from a vector of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly' [1,2,3] :: VPoly Integer
-- Poly {unPoly = [1,2,3]}
-- >>> toPoly' [0,0,0] :: UPoly Int
-- Poly {unPoly = []}
toPoly' :: (Eq a, Semiring a, G.Vector v a) => v a -> Poly v a
toPoly' = Poly . dropWhileEnd (== zero)

instance (Eq a, Num a, G.Vector v a) => Num (Poly v a) where
  Poly xs + Poly ys = toPoly $ plusPoly (+) xs ys
  Poly xs - Poly ys = toPoly $ minusPoly negate (-) xs ys
  negate (Poly xs) = Poly $ G.map negate xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> Poly $ G.empty
    m -> Poly $ G.singleton m
  Poly xs * Poly ys = toPoly $ convolution 0 (+) (*) xs ys

instance (Eq a, Semiring a, G.Vector v a) => Semiring (Poly v a) where
  zero = Poly G.empty
  one
    | (one :: a) == zero = zero
    | otherwise = Poly $ G.singleton one
  plus (Poly xs) (Poly ys) = toPoly' $ plusPoly plus xs ys
  times (Poly xs) (Poly ys) = toPoly' $ convolution zero plus times xs ys

instance (Eq a, Semiring.Ring a, G.Vector v a) => Semiring.Ring (Poly v a) where
  negate (Poly xs) = Poly $ G.map Semiring.negate xs

dropWhileEnd
  :: G.Vector v a
  => (a -> Bool)
  -> v a
  -> v a
dropWhileEnd p xs = G.basicUnsafeSlice 0 (go (G.basicLength xs)) xs
  where
    go 0 = 0
    go n = if p (G.unsafeIndex xs (n - 1)) then go (n - 1) else n

plusPoly
  :: G.Vector v a
  => (a -> a -> a)
  -> v a
  -> v a
  -> v a
plusPoly add xs ys = runST $ do
  zs <- MG.new (G.basicLength xs `max` G.basicLength ys)
  plusPolyM add xs ys zs
  G.unsafeFreeze zs

plusPolyM
  :: (PrimMonad m, G.Vector v a)
  => (a -> a -> a)
  -> v a
  -> v a
  -> G.Mutable v (PrimState m) a
  -> m ()
plusPolyM add xs ys zs = do
  let lenXs = G.basicLength xs
      lenYs = G.basicLength ys
  case lenXs `compare` lenYs of
    LT -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MG.unsafeWrite zs i (add (G.unsafeIndex xs i) (G.unsafeIndex ys i))
      G.unsafeCopy
        (MG.basicUnsafeSlice lenXs (lenYs - lenXs) zs)
        (G.basicUnsafeSlice  lenXs (lenYs - lenXs) ys)
    EQ -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MG.unsafeWrite zs i (add (G.unsafeIndex xs i) (G.unsafeIndex ys i))
    GT -> do
      forM_ [0 .. lenYs - 1] $ \i ->
        MG.unsafeWrite zs i (add (G.unsafeIndex xs i) (G.unsafeIndex ys i))
      G.unsafeCopy
        (MG.basicUnsafeSlice lenYs (lenXs - lenYs) zs)
        (G.basicUnsafeSlice  lenYs (lenXs - lenYs) xs)

minusPoly
  :: G.Vector v a
  => (a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
minusPoly neg sub xs ys = runST $ do
  zs <- MG.new (G.basicLength xs `max` G.basicLength ys)
  minusPolyM neg sub xs ys zs
  G.unsafeFreeze zs

minusPolyM
  :: (PrimMonad m, G.Vector v a)
  => (a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> G.Mutable v (PrimState m) a
  -> m ()
minusPolyM neg sub xs ys zs = do
  let lenXs = G.basicLength xs
      lenYs = G.basicLength ys
  case lenXs `compare` lenYs of
    LT -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MG.unsafeWrite zs i (sub (G.unsafeIndex xs i) (G.unsafeIndex ys i))
      forM_ [lenXs .. lenYs - 1] $ \i ->
        MG.unsafeWrite zs i (neg (G.unsafeIndex ys i))
    EQ -> do
      forM_ [0 .. lenXs - 1] $ \i ->
        MG.unsafeWrite zs i (sub (G.unsafeIndex xs i) (G.unsafeIndex ys i))
    GT -> do
      forM_ [0 .. lenYs - 1] $ \i ->
        MG.unsafeWrite zs i (sub (G.unsafeIndex xs i) (G.unsafeIndex ys i))
      G.unsafeCopy
        (MG.basicUnsafeSlice lenYs (lenXs - lenYs) zs)
        (G.basicUnsafeSlice  lenYs (lenXs - lenYs) xs)

convolution
  :: G.Vector v a
  => a
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
convolution zer add mul xs ys
  | G.null xs || G.null ys = G.empty
  | otherwise = runST $ do
    zs <- MG.new lenZs
    forM_ [0 .. lenZs - 1] $ \k -> do
      let is = [max (k - lenYs + 1) 0 .. min k (lenXs - 1)]
          acc = foldl' add zer $ flip map is $ \i ->
            mul (G.unsafeIndex xs i) (G.unsafeIndex ys (k - i))
      MG.unsafeWrite zs k acc
    G.unsafeFreeze zs
  where
    lenXs = G.basicLength xs
    lenYs = G.basicLength ys
    lenZs = lenXs + lenYs - 1

-- | Create a polynomial from a constant term.
--
-- >>> constant 42 :: UPoly Int
-- Poly {unPoly = [42]}
-- >>> constant 0 :: UPoly Int
-- Poly {unPoly = []}
constant :: (Eq a, Num a, G.Vector v a) => a -> Poly v a
constant 0 = Poly G.empty
constant c = Poly $ G.singleton c

-- | Create a polynomial from a constant term.
--
-- >>> constant' True :: UPoly Bool
-- Poly {unPoly = [True]}
-- >>> constant' False :: UPoly Bool
-- Poly {unPoly = []}
constant' :: (Eq a, Semiring a, G.Vector v a) => a -> Poly v a
constant' c
  | c == zero = Poly G.empty
  | otherwise = Poly $ G.singleton c

data StrictPair a b = !a :*: !b

infixr 1 :*:

fst' :: StrictPair a b -> a
fst' (a :*: _) = a

-- | Evaluate at a given point.
--
-- >>> -- 1 + 3^2 = 10
-- >>> eval (1 + X^2 :: UPoly Int) 3
-- 10
-- >>> -- 1 + (1 + x)^2 = 2 + 2 * x + x^2
-- >>> eval (1 + X^2 :: VPoly (UPoly Int)) (1 + X)
-- Poly {unPoly = [2,2,1]}
eval :: (Num a, G.Vector v a) => Poly v a -> a -> a
eval (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> (acc + cn * xn :*: x * xn)) (0 :*: 1) cs

-- | Evaluate at a given point.
--
-- >>> -- 1 + 3^2 = 10
-- >>> eval' (1 + X'^2 :: UPoly Int) 3
-- 10
-- >>> -- 1 + (1 + x)^2 = 2 + 2 * x + x^2
-- >>> eval' (1 + X'^2 :: VPoly (UPoly Int)) (1 + X')
-- Poly {unPoly = [2,2,1]}
eval' :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval' (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> (acc `plus` cn `times` xn :*: x `times` xn)) (zero :*: one) cs

-- | Take a derivative.
--
-- >>> -- (3 * x + x^3)' = 3 + 3 * x^2
-- >>> deriv (3 * X + X^3) :: UPoly Int
-- Poly {unPoly = [3,0,3]}
deriv :: (Eq a, Num a, G.Vector v a) => Poly v a -> Poly v a
deriv (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly $ G.imap (\i x -> fromIntegral (i + 1) * x) $ G.tail xs

-- | Take a derivative.
--
-- >>> -- (3 * x + x^3)' = 3 + 3 * x^2
-- >>> deriv' (3 * X' + X'^3) :: UPoly Int
-- Poly {unPoly = [3,0,3]}
deriv' :: (Eq a, Semiring a, G.Vector v a) => Poly v a -> Poly v a
deriv' (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly' $ G.imap (\i x -> getAdd (stimes (i + 1) (Add x))) $ G.tail xs

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (constant 3.0 + constant 3.0 * X^2) :: UPoly Double
-- Poly {unPoly = [0.0,3.0,0.0,1.0]}
integral :: (Eq a, Fractional a, G.Vector v a) => Poly v a -> Poly v a
integral (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly $ runST $ do
    zs <- MG.new (lenXs + 1)
    MG.unsafeWrite zs 0 0
    forM_ [0 .. lenXs - 1] $ \i ->
      MG.unsafeWrite zs (i + 1) (G.unsafeIndex xs i * recip (fromIntegral i + 1))
    G.unsafeFreeze zs
    where
      lenXs = G.basicLength xs

-- | Create an identity polynomial.
pattern X :: (Eq a, Num a, G.Vector v a, Eq (v a)) => Poly v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v a, Eq (v a)) => Poly v a
var
  | (1 :: a) == 0 = Poly G.empty
  | otherwise     = Poly $ G.fromList [0, 1]

-- | Create an identity polynomial.
pattern X' :: (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
pattern X' <- ((==) var' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
var'
  | (one :: a) == zero = Poly G.empty
  | otherwise          = Poly $ G.fromList [zero, one]
