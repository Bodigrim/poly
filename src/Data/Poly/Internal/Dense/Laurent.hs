-- |
-- Module:     Data.Poly.Internal.Laurent
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- Laurent polynomials of one variable.
--
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}


module Data.Poly.Internal.Dense.Laurent
  ( Laurent(..)
  , ULaurent
  , VLaurent
  , leading
  -- * Num interface
  , (^-)
  , toLaurent
  , monomial
  , scale
  , pattern X
  , eval
  -- , subst
  , deriv
  , integral
  , normalizeLaurent

   -- * Semiring interface
  , (-^)
  , toLaurent'
  , monomial'
  , scale'
  , pattern X'
  , eval'
  -- , subst'
  , deriv'
  , integral'
  , normalizeLaurent'
  )
where

import Prelude hiding (quotRem, quot, rem, gcd)
import Control.DeepSeq             (NFData)
import Control.Monad
import Control.Monad.ST
import Data.Euclidean
import Data.List (intersperse)
import Data.Poly.Internal.Dense (Poly (..), toPoly, toPoly')
import qualified Data.Poly.Internal.Dense    as Poly
import Data.Poly.Internal.Dense.Field ()
import Data.Semiring (Semiring (..))
import qualified Data.Semiring as Semiring
import qualified Data.Semiring as S
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed as U
import GHC.Generics (Generic)

data Laurent v a = Laurent !Int !(Poly v a)
  deriving (Eq, Ord, Generic)
  deriving anyclass (NFData)

type VLaurent a = Laurent V.Vector a
type ULaurent a = Laurent U.Vector a

instance (Eq a, Num a, G.Vector v a) => Num (Laurent v a) where
  Laurent n xs * Laurent m ys = Laurent (n + m) (xs * ys)
  Laurent n xs + Laurent m ys
    | n <= m    = Laurent n (xs + Poly.scale (fromIntegral (m - n)) 1 ys)
    | otherwise = Laurent m (Poly.scale (fromIntegral (n - m)) 1 xs + ys)
  negate (Laurent n xs) = Laurent n (negate xs)
  abs    = id
  signum = const 1
  fromInteger n = Laurent 0 (fromInteger n)
  {-# INLINE (+) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

instance (Eq a, Eq (v a), Field a, Num a,  G.Vector v a) => GcdDomain (Laurent v a) where
instance (Eq a, Eq (v a), Field a, Num a, G.Vector v a) => Euclidean (Laurent v a) where
  degree (Laurent n (Poly xs)) = fromIntegral (n + G.length xs)

  quotRem (normalizeLaurent -> Laurent n p@(Poly (G.toList -> xs))) (normalizeLaurent -> Laurent m q@(Poly (G.toList -> ys)))
        = if lenDiff < 0
          then  let (t, r) = toPoly (G.fromList (replicate (abs lenDiff) 0 ++ xs)) `quotRem` q
                in ( Laurent (n - m + lenDiff) t
                   , Laurent (n + lenDiff) r
                   )
          else  let (t, r) = p `quotRem` q
                in ( Laurent (n - m) t
                   , Laurent n r
                   )
          where
            lenDiff = length xs - length ys
  {-# INLINE quotRem #-}
  rem a b = snd $ quotRem a b
  {-# INLINE rem #-}

instance (Show a, G.Vector v a) => Show (Laurent v a) where
  showsPrec d (Laurent n (Poly xs))
    | G.null xs
      = showString "0"
    | G.length xs == 1 && n == 0
      = showsPrec d (G.head xs)
    | otherwise
      = showParen (d > 0)
      $ foldl (.) id
      $ intersperse (showString " + ")
      $ G.ifoldl (\acc i c -> showCoeff (i + n) c : acc) [] xs
    where
      showCoeff 0 c = showsPrec 7 c
      showCoeff 1 c = showsPrec 7 c . showString " * X"
      showCoeff i c = showsPrec 7 c . showString " * X^" . showsPrec 7 i

instance (Eq a, Semiring a, G.Vector v a) => Semiring (Laurent v a) where
  zero = Laurent 0 zero
  one  = Laurent 0 one

  Laurent n xs `times` Laurent m ys = Laurent (n + m) (xs `times` ys)
  Laurent n xs `plus` Laurent m ys
    | n <= m    = Laurent n (xs `plus` Poly.scale' (fromIntegral (m - n)) one ys)
    | otherwise = Laurent m (Poly.scale' (fromIntegral (n - m)) one xs `plus` ys)
  {-# INLINE zero #-}
  {-# INLINE one #-}
  {-# INLINE plus #-}
  {-# INLINE times #-}

normalizeLaurent :: (Eq a, Num a, G.Vector v a) => Laurent v a -> Laurent v a
normalizeLaurent (Laurent n (Poly (G.toList -> xs))) = go n xs
  where
    go _ [] = 0
    go e (y:ys)
      | e < 0 && y == 0 = go (e + 1) ys
      | e > 0 = Laurent 0 (toPoly $ G.fromList $ replicate n 0)
      | otherwise = Laurent e (toPoly $ G.fromList (y:ys))

normalizeLaurent' :: (Eq a, Semiring a, G.Vector v a) => Laurent v a -> Laurent v a
normalizeLaurent' (Laurent n (Poly (G.toList -> xs))) = go n xs
  where
    go _ [] = zero
    go e (y:ys)
      | e < 0 && y == zero = go (e `plus` 1) ys
      | e > 0 = Laurent 0 (toPoly' $ G.fromList $ replicate n zero)
      | otherwise = Laurent e (toPoly' $ G.fromList (y:ys))

toLaurent :: (Eq a, Num a, G.Vector v a) => v a -> Laurent v a
toLaurent = Laurent 0 . toPoly

toLaurent' :: (Eq a, Semiring a, G.Vector v a) => v a -> Laurent v a
toLaurent' = Laurent 0 . toPoly'

-- | Create a monomial from a power and a coefficient.
monomial :: (Eq a, Num a, G.Vector v a) => Int -> a -> Laurent v a
monomial _ 0 = Laurent 0 (Poly G.empty)
monomial p c
  | p >= 0     = Laurent 0 (Poly.monomial (fromIntegral p) c)
  | otherwise  = Laurent p (Poly $ G.singleton c)
{-# INLINE monomial #-}

monomial' :: (Eq a, Semiring a, G.Vector v a) => Int -> a -> Laurent v a
monomial' p c
  | c == zero  = Laurent 0 (Poly G.empty)
  | p >= 0     = Laurent 0 (Poly.monomial' (fromIntegral p) c)
  | otherwise  = Laurent p (Poly $ G.singleton c)
{-# INLINE monomial' #-}

-- | Multiply a polynomial by a monomial, expressed as a power and a coefficient.
--
-- >>> scale (-5) 3 (X^-2 + 1) :: UPoly Int
-- 3 * X^4 + 0 * X^3 + 3 * X^2 + 0 * X + 0
scale :: (Eq a, Num a, G.Vector v a) => Int -> a -> Laurent v a -> Laurent v a
scale yp yc (Laurent n (Poly xs)) = normalizeLaurent $ Laurent (n+yp) (toPoly $ scaleInternal 0 (*) yp yc xs)

scale' :: (Eq a, Semiring a, G.Vector v a) => Int -> a -> Laurent v a -> Laurent v a
scale' yp yc (Laurent n (Poly xs)) = normalizeLaurent' $ Laurent (n+yp) (toPoly' $ scaleInternal zero times yp yc xs)

scaleInternal
  :: (Eq a, G.Vector v a)
  => a
  -> (a -> a -> a)
  -> Int
  -> a
  -> v a
  -> v a
scaleInternal zer mul yp yc xs
  | yp >= 0 = runST $ do
    let lenXs = G.length xs
    zs <- MG.unsafeNew (fromIntegral yp + lenXs)
    forM_ [0 .. fromIntegral yp - 1] $ \k ->
      MG.unsafeWrite zs k zer
    forM_ [0 .. lenXs - 1] $ \k ->
      MG.unsafeWrite zs (fromIntegral yp + k) (mul yc $ G.unsafeIndex xs k)
    G.unsafeFreeze zs
  | otherwise = runST $ do
    let lenXs = G.length xs
    zs <- MG.unsafeNew lenXs
    forM_ [0 .. lenXs - 1] $ \k ->
      MG.unsafeWrite zs k (mul yc $ G.unsafeIndex xs k)
    G.unsafeFreeze zs

{-# INLINABLE scaleInternal #-}

-- | Create an identity polynomial.
pattern X :: (Eq a, Num a, G.Vector v a, Eq (v a)) => Laurent v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v a, Eq (v a)) => Laurent v a
var
  | (1 :: a) == 0 = Laurent 0 (Poly G.empty)
  | otherwise     = Laurent 0 (Poly $ G.fromList [0, 1])
{-# INLINE var #-}

-- | Create an identity polynomial.
pattern X' :: (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Laurent v a
pattern X' <- ((==) var' -> True)
  where X' = var'

var' :: forall a v. (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Laurent v a
var'
  | (one :: a) == zero = Laurent 0 (Poly G.empty)
  | otherwise          = Laurent 0 (Poly $ G.fromList [zero, one])
{-# INLINE var' #-}


-- | Evaluate at a given point.
--
-- >>> eval (X^-2 + 1 :: UPoly Rational) 3
-- 10
eval :: (Num a, Fractional a, G.Vector v a, G.Vector v (Int, a), G.Vector v Int) => Laurent v a -> a -> a
eval = substitute (*)
{-# INLINE eval #-}

eval' :: (S.Ring a, Fractional a, G.Vector v a, G.Vector v (Int, a), G.Vector v Int) => Laurent v a -> a -> a
eval' = substitute' times
{-# INLINE eval' #-}

data StrictPair a b = !a :*: !b

infixr 1 :*:

fst' :: StrictPair a b -> a
fst' (a :*: _) = a

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
-- subst :: (Eq a, Eq (v a), Integral a, Fractional a, G.Vector v a, G.Vector v (Int, a), G.Vector v Int) => Laurent v a -> Laurent v a -> Laurent v a
-- subst = substitute (scale 0)
-- {-# INLINE subst #-}
--
-- subst' :: (Eq a, Eq (v a), S.Ring a, G.Vector v a, G.Vector v (Int, a), G.Vector v Int) => Laurent v a -> Laurent v a -> Laurent v a
-- subst' = substitute' (scale' zero)
-- {-# INLINE subst' #-}

substitute :: (Num b, Fractional b, G.Vector v a, G.Vector v (Int, a), G.Vector v Int)
  => (a -> b -> b)
  -> Laurent v a
  -> b
  -> b
substitute f (Laurent n (Poly cs)) x
  | n >= 0 = fst' $
      G.foldl' (\(acc :*: xn) cn -> acc + f cn xn :*: x * xn) (0 :*: 1) cs
  | otherwise = fst' $
      G.foldl' (\(acc :*: xn) (i, cn) -> acc + f cn xn :*: pow x i)
                (0 :*: pow x n)
                (G.zip (G.fromList [(n+1)..(n + G.basicLength cs)]) cs)
  where
    pow b i
      | i < 0 = recip b ^ negate i
      | otherwise = b ^ i
{-# INLINE substitute #-}

substitute' :: (G.Vector v a, G.Vector v (Int, a), G.Vector v Int, S.Ring b) => (a -> b -> b) -> Laurent v a -> b -> b
substitute' f (Laurent n (Poly cs)) x
  | n >= 0 = fst' $
      G.foldl' (\(acc :*: xn) cn -> acc `plus` f cn xn :*: x `times` xn) (zero :*: one) cs
  | otherwise = fst' $
      G.foldl' (\(acc :*: xn) (i, cn) -> acc `plus` f cn xn :*: pow x i)
                (zero :*: pow x n)
                (G.zip (G.fromList [(n+1)..(n + G.basicLength cs)]) cs)
  where
    pow b i
      | i < 0 = S.negate b S.^ negate i
      | otherwise = b S.^ i
{-# INLINE substitute' #-}
-- | Return a leading power and coefficient of a non-zero Laurent polynomial.
--
-- >>> leading ((2 * X^-1 + 1) * (2 * X^-2 - 1) :: UPoly Int)
-- Just (0,-1)
-- >>> leading (0 :: UPoly Int)
-- Nothing
leading :: G.Vector v a => Laurent v a -> Maybe (Int, a)
leading (Laurent n (Poly v))
  | G.null v  = Nothing
  | otherwise = Just (n + G.length v - 1, G.last v)

-- | Exponentiation of an identity Laurent polynomial to a negative integer.
infixr 8 ^-
(^-) :: (Eq a, Num a, G.Vector v a, Eq (v a)) => Laurent v a -> Int -> Laurent v a
X ^- n = monomial (-n) 1
_ ^- _ = error "(^-): not the identity Laurent polynomial"
{-# INLINE (^-) #-}

infixr 8 -^
(-^) :: (Eq a, Semiring a, G.Vector v a, Eq (v a)) => Laurent v a -> Int -> Laurent v a
X' -^ n = monomial' (-n) one
_  -^ _ = error "(-^): not the identity Laurent polynomial"
{-# INLINE (-^) #-}

-- | Take a derivative.
--
-- >>> deriv (X^-3 + 3 * X) :: UPoly Int
-- 2 * X + 0 + 0 * X^(-1) + 0 * X^(-2) + 0 * X^(-3) + (-3) * X^(-4)
deriv :: (Eq a, Num a, G.Vector v a) => Laurent v a -> Laurent v a
deriv (Laurent n (Poly xs))
  | G.null xs = Laurent 0 (Poly G.empty)
  | otherwise = Laurent (n-1) (toPoly $ G.imap (\i x -> fromIntegral (n + i) * x) xs)
{-# INLINE deriv #-}

deriv' :: (Eq a, Semiring a, G.Vector v a) => Laurent v a -> Laurent v a
deriv' (Laurent n (Poly xs))
  | G.null xs = Laurent 0 (Poly G.empty)
  | otherwise = Laurent (n-1) (toPoly' $ G.imap (\i x -> fromNatural (fromIntegral (n + i)) `times` x) xs)
{-# INLINE deriv' #-}

-- | Compute an indefinite integral of a polynomial,
-- setting constant term to zero.
--
-- TODO: How to handle logarithms?
-- >>> integral (X^-2 + 3) :: UPoly Double
-- 3.0 * X + NaN + (-1.0) * X^(-1) + 0.0 * X^(-2)
integral :: (Eq a, Fractional a, G.Vector v a) => Laurent v a -> Laurent v a
integral (Laurent n (Poly xs))
  | G.null xs = Laurent 0 (Poly G.empty)
  | otherwise = Laurent n $ toPoly $ runST $ do
    zs <- MG.unsafeNew (lenXs + 1)
    MG.unsafeWrite zs 0 0
    forM_ [0 .. lenXs - 1] $ \i ->
      MG.unsafeWrite zs (i + 1) (G.unsafeIndex xs i * recip (fromIntegral n + fromIntegral i + 1))
    G.unsafeFreeze zs
    where
      lenXs = G.length xs
{-# INLINABLE integral #-}

#if MIN_VERSION_semirings(0,5,0)
integral' :: (Eq a, Field a, G.Vector v a) => Laurent v a -> Laurent v a
integral' (Laurent n (Poly xs))
  | G.null xs = Laurent 0 (Poly G.empty)
  | otherwise = Laurent n $ toPoly' $ runST $ do
    zs <- MG.unsafeNew (lenXs + 1)
    MG.unsafeWrite zs zero zero
    forM_ [0 .. lenXs - 1] $ \i ->
      MG.unsafeWrite zs (i + 1) (G.unsafeIndex xs i `quot` Semiring.fromIntegral (n + i + 1))
    G.unsafeFreeze zs
    where
      lenXs = G.length xs
{-# INLINABLE integral' #-}
#endif