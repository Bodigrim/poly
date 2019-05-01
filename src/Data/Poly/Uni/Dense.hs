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
  , quotRem
  -- * Semiring interface
  , toPoly'
  , constant'
  , pattern X'
  , eval'
  , deriv'
  ) where

import Prelude hiding (quotRem)
import Control.Exception
import Control.Monad
import Control.Monad.ST
import Data.List (foldl', intersperse)
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
  deriving (Eq, Ord)

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
  let lenXs = G.basicLength xs
      lenYs = G.basicLength ys
      lenMn = lenXs `min` lenYs
      lenMx = lenXs `max` lenYs

  zs <- MG.basicUnsafeNew lenMx
  forM_ [0 .. lenMn - 1] $ \i ->
    MG.unsafeWrite zs i (add (G.unsafeIndex xs i) (G.unsafeIndex ys i))
  G.unsafeCopy
    (MG.basicUnsafeSlice lenMn (lenMx - lenMn) zs)
    (G.basicUnsafeSlice  lenMn (lenMx - lenMn) (if lenXs <= lenYs then ys else xs))

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
  let lenXs = G.basicLength xs
      lenYs = G.basicLength ys
      lenMn = lenXs `min` lenYs
      lenMx = lenXs `max` lenYs

  zs <- MG.basicUnsafeNew lenMx
  forM_ [0 .. lenMn - 1] $ \i ->
    MG.unsafeWrite zs i (sub (G.unsafeIndex xs i) (G.unsafeIndex ys i))

  if lenXs < lenYs
    then forM_ [lenXs .. lenYs - 1] $ \i ->
      MG.unsafeWrite zs i (neg (G.unsafeIndex ys i))
    else G.unsafeCopy
      (MG.basicUnsafeSlice lenYs (lenXs - lenYs) zs)
      (G.basicUnsafeSlice  lenYs (lenXs - lenYs) xs)

  G.unsafeFreeze zs
{-# INLINE minusPoly #-}

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
    let lenXs = G.basicLength xs
        lenYs = G.basicLength ys
        lenZs = lenXs + lenYs - 1
    zs <- MG.basicUnsafeNew lenZs
    forM_ [0 .. lenZs - 1] $ \k -> do
      let is = [max (k - lenYs + 1) 0 .. min k (lenXs - 1)]
          acc = foldl' add zer $ flip map is $ \i ->
            mul (G.unsafeIndex xs i) (G.unsafeIndex ys (k - i))
      MG.unsafeWrite zs k acc
    G.unsafeFreeze zs
{-# INLINE convolution #-}

-- | This is just a proof of concept,
-- which should be replaced by a proper 'Euclidean' interface.
quotRem
  :: (Integral a, G.Vector v a)
  => Poly v a
  -> Poly v a
  -> (Poly v a, Poly v a)
quotRem (Poly xs) (Poly ys) = (toPoly qs, toPoly rs)
  where
    (qs, rs) = quotRem' xs ys

quotRem'
  :: (Integral a, G.Vector v a)
  => v a
  -> v a
  -> (v a, v a)
quotRem' xs ys
  | G.null ys = throw DivideByZero
  | G.basicLength xs < G.basicLength ys = (G.empty, xs)
  | otherwise = runST $ do
    let lenXs = G.basicLength xs
        lenYs = G.basicLength ys
        lenQs = lenXs - lenYs + 1
    qs <- MG.new lenQs
    rs <- MG.new lenXs
    G.unsafeCopy rs xs
    forM_ [lenQs - 1, lenQs - 2 .. 0] $ \i -> do
      let j = lenXs - 1 + i - (lenQs - 1)
      r <- MG.unsafeRead rs j
      let q = r `quot` G.unsafeLast ys
      MG.unsafeWrite qs i q
      forM_ [0 .. lenYs - 1] $ \k -> do
        MG.unsafeModify rs (\c -> c - q * G.unsafeIndex ys k) (j + k - lenYs + 1)
    (,) <$> G.unsafeFreeze qs <*> G.unsafeFreeze rs


-- | Create a polynomial from a constant term.
constant :: (Eq a, Num a, G.Vector v a) => a -> Poly v a
constant 0 = Poly G.empty
constant c = Poly $ G.singleton c

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
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
-- >>> eval (X^2 + 1 :: VPoly (UPoly Int)) (X + 1)
-- 1 * X^2 + 2 * X + 2
eval :: (Num a, G.Vector v a) => Poly v a -> a -> a
eval (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> (acc + cn * xn :*: x * xn)) (0 :*: 1) cs
{-# INLINE eval #-}

eval' :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval' (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> (acc `plus` cn `times` xn :*: x `times` xn)) (zero :*: one) cs
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
  | otherwise = toPoly' $ G.imap (\i x -> getAdd (stimes (i + 1) (Add x))) $ G.tail xs
{-# INLINE deriv' #-}

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- >>> integral (constant 3.0 * X^2 + constant 3.0) :: UPoly Double
-- 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
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
{-# INLINE integral #-}

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
