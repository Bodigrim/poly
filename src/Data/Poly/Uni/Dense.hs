-- |
-- Module:      Data.Poly.Uni.Dense
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials of one variable.
--

{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Poly.Uni.Dense
  ( Poly
  , unPoly
  -- * Num interface
  , toPoly
  , constant
  , eval
  , deriv
  , pattern X
  , integral
  -- * Semiring interface
  , toPoly'
  , constant'
  , eval'
  , deriv'
  , pattern X'
  ) where

import Control.Monad
import Control.Monad.ST
import Data.List (foldl')
import Data.Semigroup (stimes)
import Data.Semiring (Semiring(..), Add(..))
import qualified Data.Semiring as Semiring
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG

-- | Polynomials of one variable.
--
-- >>> :set -XOverloadedLists
-- >>> -- (1 + x) * (-1 + x) = (-1 + x^2)
-- >>> toPoly [1,1] * toPoly [-1,1]
-- Poly {unPoly = [-1,0,1]}
--
-- >>> :set -XOverloadedLists
-- >>> -- (1 + x) + (1 - x) = 2
-- >>> toPoly [1,1] + toPoly [1,-1]
-- Poly {unPoly = [2]}
newtype Poly v a = Poly
  { unPoly :: v a
  -- ^ Convert 'Poly' to a vector of coefficients
  -- (first element corresponds to a constant term).
  }
  deriving (Eq, Ord, Show)

-- | Make 'Poly' from a list of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3]
-- Poly {unPoly = [1,2,3]}
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [0,0,0]
-- Poly {unPoly = []}
toPoly :: (Eq a, Num a, G.Vector v a) => v a -> Poly v a
toPoly = Poly . dropWhileEnd (== 0)

-- | Make 'Poly' from a vector of coefficients
-- (first element corresponds to a constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly' [1,2,3]
-- Poly {unPoly = [1,2,3]}
--
-- >>> :set -XOverloadedLists
-- >>> toPoly' [0,0,0]
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

dropWhileEnd :: G.Vector v a => (a -> Bool) -> v a -> v a
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
  zs <- MG.new (max lenXs lenYs)
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
  G.unsafeFreeze zs
  where
    lenXs = G.basicLength xs
    lenYs = G.basicLength ys

minusPoly
  :: G.Vector v a
  => (a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
minusPoly neg sub xs ys = runST $ do
  zs <- MG.new (max lenXs lenYs)
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
  G.unsafeFreeze zs
  where
    lenXs = G.basicLength xs
    lenYs = G.basicLength ys

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
          -- js = reverse [max (k - lenXs) 0 .. min k lenYs]
      let acc = foldl' add zer $ flip map is $ \i ->
            mul (G.unsafeIndex xs i) (G.unsafeIndex ys (k - i))
      MG.unsafeWrite zs k acc
    G.unsafeFreeze zs
  where
    lenXs = G.basicLength xs
    lenYs = G.basicLength ys
    lenZs = lenXs + lenYs - 1

constant :: (Eq a, Num a, G.Vector v a) => a -> Poly v a
constant 0 = Poly G.empty
constant c = Poly $ G.singleton c

constant' :: (Eq a, Semiring a, G.Vector v a) => a -> Poly v a
constant' c
  | c == zero = Poly G.empty
  | otherwise = Poly $ G.singleton c

eval :: (Num a, G.Vector v a) => Poly v a -> a -> a
eval (Poly cs) x = fst $
  G.foldl' (\(acc, xn) cn -> (acc + cn * xn, x * xn)) (0, 1) cs

eval' :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval' (Poly cs) x = fst $
  G.foldl' (\(acc, xn) cn -> (acc `plus` cn `times` xn, x `times` xn)) (zero, one) cs

deriv :: (Eq a, Num a, G.Vector v a) => Poly v a -> Poly v a
deriv (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly $ G.imap (\i x -> fromIntegral (i + 1) * x) $ G.tail xs

deriv' :: (Eq a, Semiring a, G.Vector v a) => Poly v a -> Poly v a
deriv' (Poly xs)
  | G.null xs = Poly G.empty
  | otherwise = toPoly' $ G.imap (\i x -> getAdd (stimes (i + 1) (Add x))) $ G.tail xs

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

pattern X :: (Eq a, Num a, G.Vector v a, Eq (v a)) => Poly v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v a, Eq (v a)) => Poly v a
var
  | (1 :: a) == 0 = Poly G.empty
  | otherwise     = Poly $ G.fromList [0, 1]

pattern X' :: (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
pattern X' <- ((==) var' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Poly v a
var'
  | (one :: a) == zero = Poly G.empty
  | otherwise          = Poly $ G.fromList [zero, one]
