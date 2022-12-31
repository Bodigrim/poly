-- |
-- Module:      Data.Poly.Internal.Dense
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- Dense polynomials of one variable.
--

{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
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
  , subst
  , deriv
  , integral
  -- * Semiring interface
  , toPoly'
  , monomial'
  , scale'
  , pattern X'
  , eval'
  , subst'
  , substitute'
  , deriv'
  , unscale'
  , integral'
  , timesRing
  ) where

import Prelude hiding (quotRem, quot, rem, gcd, lcm)
import Control.DeepSeq (NFData)
import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Euclidean (Euclidean, Field, quot)
import Data.Kind
import Data.List (foldl', intersperse)
import Data.Semiring (Semiring(..), Ring(), minus)
import qualified Data.Semiring as Semiring
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import GHC.Exts

-- | Polynomials of one variable with coefficients from @a@,
-- backed by a 'G.Vector' @v@ (boxed, unboxed, storable, etc.).
--
-- Use the pattern 'X' for construction:
--
-- >>> (X + 1) + (X - 1) :: VPoly Integer
-- 2 * X + 0
-- >>> (X + 1) * (X - 1) :: UPoly Int
-- 1 * X^2 + 0 * X + (-1)
--
-- Polynomials are stored normalized, without leading
-- zero coefficients, so 0 * 'X' + 1 equals to 1.
--
-- The 'Ord' instance does not make much sense mathematically,
-- it is defined only for the sake of 'Data.Set.Set', 'Data.Map.Map', etc.
--
-- Due to being polymorphic by multiple axis, the performance of `Poly` crucially
-- depends on specialisation of instances. Clients are strongly recommended
-- to compile with @ghc-options:@ @-fspecialise-aggressively@ and suggested to enable @-O2@.
--
-- @since 0.1.0.0
newtype Poly (v :: Type -> Type) (a :: Type) = Poly
  { unPoly :: v a
  -- ^ Convert a 'Poly' to a vector of coefficients
  -- (first element corresponds to the constant term).
  --
  -- @since 0.1.0.0
  }
  deriving
  ( Eq, Ord
  , NFData -- ^ @since 0.3.2.0
  )

-- | @since 0.3.1.0
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
      -- Powers are guaranteed to be non-negative
      showCoeff :: Int -> a -> String -> String
      showCoeff 0 c = showsPrec 7 c
      showCoeff 1 c = showsPrec 7 c . showString " * X"
      showCoeff i c = showsPrec 7 c . showString (" * X^" ++ show i)

-- | Polynomials backed by boxed vectors.
--
-- @since 0.2.0.0
type VPoly = Poly V.Vector

-- | Polynomials backed by unboxed vectors.
--
-- @since 0.2.0.0
type UPoly = Poly U.Vector

-- | Make a 'Poly' from a list of coefficients
-- (first element corresponds to the constant term).
--
-- >>> :set -XOverloadedLists
-- >>> toPoly [1,2,3] :: VPoly Integer
-- 3 * X^2 + 2 * X + 1
-- >>> toPoly [0,0,0] :: UPoly Int
-- 0
--
-- @since 0.1.0.0
toPoly :: (Eq a, Num a, G.Vector v a) => v a -> Poly v a
toPoly = Poly . dropWhileEnd (== 0)
{-# INLINABLE toPoly #-}

toPoly' :: (Eq a, Semiring a, G.Vector v a) => v a -> Poly v a
toPoly' = Poly . dropWhileEnd (== zero)
{-# INLINABLE toPoly' #-}

-- | Return the leading power and coefficient of a non-zero polynomial.
--
-- >>> leading ((2 * X + 1) * (2 * X^2 - 1) :: UPoly Int)
-- Just (3,4)
-- >>> leading (0 :: UPoly Int)
-- Nothing
--
-- @since 0.3.0.0
leading :: G.Vector v a => Poly v a -> Maybe (Word, a)
leading (Poly v)
  | G.null v  = Nothing
  | otherwise = Just (fromIntegral (G.length v - 1), G.last v)
{-# INLINABLE leading #-}

-- | Note that 'abs' = 'id' and 'signum' = 'const' 1.
instance (Eq a, Num a, G.Vector v a) => Num (Poly v a) where
  (+) = (toPoly .) . coerce (plusPoly @v @a (+))
  (-) = (toPoly .) . coerce (minusPoly @v @a negate (-))
  (*) = (toPoly .) . coerce (inline (karatsuba @v @a 0 (+) (-) (*)))

  negate (Poly xs) = Poly $ G.map negate xs
  abs = id
  signum = const 1
  fromInteger n = case fromInteger n of
    0 -> Poly G.empty
    m -> Poly $ G.singleton m

  {-# INLINE (+) #-}
  {-# INLINE (-) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

-- | Note that 'times' is significantly slower than '(*)' for large polynomials,
-- because Karatsuba multiplication algorithm requires subtraction, which is not
-- provided by 'Semiring' class. Use 'timesRing' instead.
instance (Eq a, Semiring a, G.Vector v a) => Semiring (Poly v a) where
  zero = Poly G.empty
  one
    | (one :: a) == zero = zero
    | otherwise = Poly $ G.singleton one

  plus  = (toPoly' .) . coerce (plusPoly @v @a plus)
  times = (toPoly' .) . coerce (inline (convolution @v @a zero plus times))

  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}

  fromNatural n = if n' == zero then zero else Poly $ G.singleton n'
    where
      n' :: a
      n' = fromNatural n
  {-# INLINE fromNatural #-}

instance (Eq a, Ring a, G.Vector v a) => Ring (Poly v a) where
  negate (Poly xs) = Poly $ G.map Semiring.negate xs
  {-# INLINABLE negate #-}

-- | Karatsuba multiplication algorithm for polynomials over rings.
timesRing :: forall v a. (Eq a, Ring a, G.Vector v a) => Poly v a -> Poly v a -> Poly v a
timesRing = (toPoly' .) . coerce (inline (karatsuba @v @a zero plus minus times))
{-# INLINE timesRing #-}

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
  :: G.Vector v a
  => (a -> Bool)
  -> G.Mutable v s a
  -> ST s (G.Mutable v s a)
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
{-# INLINABLE plusPoly #-}

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
{-# INLINABLE minusPoly #-}

karatsubaThreshold :: Int
karatsubaThreshold = 32

karatsuba
  :: G.Vector v a
  => a
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
karatsuba zer add sub mul = go
  where
    conv = inline convolution zer add mul
    go xs ys
      | lenXs <= karatsubaThreshold || lenYs <= karatsubaThreshold
      = conv xs ys
      | otherwise = runST $ do
        zs <- MG.unsafeNew lenZs
        forM_ [0 .. lenZs - 1] $ \k -> do
          let z0 = if k < G.length zs0
                   then G.unsafeIndex zs0 k
                   else zer
              z11 = if k - m >= 0 && k - m < G.length zs11
                   then G.unsafeIndex zs11 (k - m)
                   else zer
              z10 = if k - m >= 0 && k - m < G.length zs0
                   then G.unsafeIndex zs0 (k - m)
                   else zer
              z12 = if k - m >= 0 && k - m < G.length zs2
                   then G.unsafeIndex zs2 (k - m)
                   else zer
              z2 = if k - 2 * m >= 0 && k - 2 * m < G.length zs2
                   then G.unsafeIndex zs2 (k - 2 * m)
                   else zer
          MG.unsafeWrite zs k (z0 `add` (z11 `sub` (z10 `add` z12)) `add` z2)
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

        xs01 = plusPoly add xs0 xs1
        ys01 = plusPoly add ys0 ys1
        zs0  = go xs0 ys0
        zs2  = go xs1 ys1
        zs11 = go xs01 ys01
{-# INLINABLE karatsuba #-}

convolution
  :: G.Vector v a
  => a
  -> (a -> a -> a)
  -> (a -> a -> a)
  -> v a
  -> v a
  -> v a
convolution zer add mul = \xs ys ->
  let lenXs = G.length xs
      lenYs = G.length ys
      lenZs = lenXs + lenYs - 1 in
  if lenXs == 0 || lenYs == 0
  then G.empty
  else G.generate lenZs $ \k -> foldl'
    (\acc i -> acc `add` mul (G.unsafeIndex xs i) (G.unsafeIndex ys (k - i)))
    zer
    [max (k - lenYs + 1) 0 .. min k (lenXs - 1)]
{-# INLINABLE convolution #-}

-- | Create a monomial from a power and a coefficient.
--
-- @since 0.3.0.0
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
  :: G.Vector v a
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
{-# INLINABLE scaleInternal #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale 2 3 (X^2 + 1) :: UPoly Int
-- 3 * X^4 + 0 * X^3 + 3 * X^2 + 0 * X + 0
--
-- @since 0.3.0.0
scale :: (Eq a, Num a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
scale yp yc (Poly xs) = toPoly $ scaleInternal 0 (*) yp yc xs

scale' :: (Eq a, Semiring a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
scale' yp yc (Poly xs) = toPoly' $ scaleInternal zero times yp yc xs

unscale' :: (Eq a, Euclidean a, G.Vector v a) => Word -> a -> Poly v a -> Poly v a
unscale' yp yc (Poly xs) = toPoly' $ runST $ do
  let lenZs = G.length xs - fromIntegral yp
  zs <- MG.unsafeNew lenZs
  forM_ [0 .. lenZs - 1] $ \k ->
    MG.unsafeWrite zs k (G.unsafeIndex xs (k + fromIntegral yp) `quot` yc)
  G.unsafeFreeze zs
{-# INLINABLE unscale' #-}

data StrictPair a b = !a :*: !b

infixr 1 :*:

fst' :: StrictPair a b -> a
fst' (a :*: _) = a

-- | Evaluate the polynomial at a given point.
--
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
--
-- @since 0.2.0.0
eval :: (Num a, G.Vector v a) => Poly v a -> a -> a
eval = substitute (*)
{-# INLINE eval #-}

eval' :: (Semiring a, G.Vector v a) => Poly v a -> a -> a
eval' = substitute' times
{-# INLINE eval' #-}

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
--
-- @since 0.3.3.0
subst :: (Eq a, Num a, G.Vector v a, G.Vector w a) => Poly v a -> Poly w a -> Poly w a
subst = substitute (scale 0)
{-# INLINE subst #-}

subst' :: (Eq a, Semiring a, G.Vector v a, G.Vector w a) => Poly v a -> Poly w a -> Poly w a
subst' = substitute' (scale' 0)
{-# INLINE subst' #-}

substitute :: (G.Vector v a, Num b) => (a -> b -> b) -> Poly v a -> b -> b
substitute f (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> acc + f cn xn :*: x * xn) (0 :*: 1) cs
{-# INLINE substitute #-}

substitute' :: (G.Vector v a, Semiring b) => (a -> b -> b) -> Poly v a -> b -> b
substitute' f (Poly cs) x = fst' $
  G.foldl' (\(acc :*: xn) cn -> acc `plus` f cn xn :*: x `times` xn) (zero :*: one) cs
{-# INLINE substitute' #-}

-- | Take the derivative of the polynomial.
--
-- >>> deriv (X^3 + 3 * X) :: UPoly Int
-- 3 * X^2 + 0 * X + 3
--
-- @since 0.2.0.0
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

-- | Compute an indefinite integral of the polynomial,
-- setting the constant term to zero.
--
-- >>> integral (3 * X^2 + 3) :: UPoly Double
-- 1.0 * X^3 + 0.0 * X^2 + 3.0 * X + 0.0
--
-- @since 0.2.0.0
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
{-# INLINABLE integral #-}

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
{-# INLINABLE integral' #-}

-- | The polynomial 'X'.
--
-- > X == monomial 1 1
--
-- @since 0.2.0.0
pattern X :: (Eq a, Num a, G.Vector v a) => Poly v a
pattern X <- (isVar -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v a) => Poly v a
var
  | (1 :: a) == 0 = Poly G.empty
  | otherwise     = Poly $ G.fromList [0, 1]
{-# INLINE var #-}

isVar :: forall v a. (Eq a, Num a, G.Vector v a) => Poly v a -> Bool
isVar (Poly xs)
  | (1 :: a) == 0 = G.null xs
  | otherwise     = G.length xs == 2 && xs G.! 0 == 0 && xs G.! 1 == 1
{-# INLINE isVar #-}

-- | Create an identity polynomial.
pattern X' :: (Eq a, Semiring a, G.Vector v a) => Poly v a
pattern X' <- (isVar' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v a) => Poly v a
var'
  | (one :: a) == zero = Poly G.empty
  | otherwise          = Poly $ G.fromList [zero, one]
{-# INLINE var' #-}

isVar' :: forall v a. (Eq a, Semiring a, G.Vector v a) => Poly v a -> Bool
isVar' (Poly xs)
  | (one :: a) == zero = G.null xs
  | otherwise          = G.length xs == 2 && xs G.! 0 == zero && xs G.! 1 == one
{-# INLINE isVar' #-}
