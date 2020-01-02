-- |
-- Module:     Data.Poly.Internal.Laurent
-- Copyright:  (c) 2019 Adjoint Inc
-- Licence:    BSD3
-- Maintainer: Adjoint Inc <info@adjoint.io>
--
-- Laurent polynomials of one variable.
--
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

--   ( Poly(..)
--   , VPoly
--   , UPoly
--   , leading
--   , dropWhileEndM
--   -- * Num interface
--   , toPoly
--   , monomial
--   , scale
--   , scaleInternal
--   , pattern X
--   , eval
--   , subst
--   , deriv
--   , integral
--   -- * Semiring interface
--   , toPoly'
--   , monomial'
--   , scale'
--   , pattern X'
--   , eval'
--   , subst'
--   , deriv'
-- #if MIN_VERSION_semirings(0,5,0)
--   , unscale'
--   , integral'
-- #endif
module Data.Poly.Internal.Dense.Laurent
  ( Laurent(..)
  , ULaurent
  , VLaurent
  , leading
  , (^-)
  -- * Num interface
  , toLaurent
  , monomial
  , scale
  , pattern X
  , eval
  , subst
  -- , deriv
  -- , integral

   -- * Semiring interface
  , toLaurent'
  , monomial'
  )
where

import           Control.DeepSeq             (NFData)
import           Control.Monad
import           Control.Monad.Primitive
import           Control.Monad.ST
import           Data.List                   (foldl', intersperse)
import           Data.Poly.Internal.Dense    (Poly (..), toPoly, toPoly')
import qualified Data.Poly.Internal.Dense    as Poly
import           Data.Semiring               (Semiring (..))
import qualified Data.Semiring               as S
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as MG
import qualified Data.Vector.Unboxed         as U
import           GHC.Generics                (Generic)

-- TODO: Move this to Internal/Dense/Laurent.hs

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

quotLaurent :: (Eq a, Integral a, G.Vector v a) => Laurent v a -> Laurent v a -> Laurent v a
quotLaurent (Laurent n p@(Poly (G.toList -> xs))) (Laurent m q@(Poly (G.toList -> ys)))
  = if lenDiff < 0
    then Laurent (n - m + lenDiff)
          (toPoly (G.fromList (replicate (abs lenDiff) 0 ++ xs)) `Poly.quotPoly` q)
    else Laurent (n - m) (p `Poly.quotPoly` q)
    where
      lenDiff = length xs - length ys

instance (Show a, G.Vector v a) => Show (Laurent v a) where
  showsPrec d (Laurent n (Poly xs))
    | G.null xs
      = showString "0"
    | G.length xs == 1
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


-- TODO: Move this to Sparse

-- toLaurent :: forall a v. (Eq a, Num a, G.Vector v a) => v (Int, a) -> Laurent v a
-- toLaurent v = Laurent deg (toPoly p)
--   where
--     deg :: Int
--     deg = G.foldl' (\i (j, _) -> min i j) 0 v
--     p :: v (Word, a)
--     p = G.map (\(i, a) -> (fromIntegral (i + deg), a)) v

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
-- TODO: Fix this
scale :: (Eq a, Num a, G.Vector v a) => Int -> a -> Laurent v a -> Laurent v a
scale yp yc (Laurent n (Poly xs)) = Laurent n (toPoly $ scaleInternal 0 (*) yp yc xs)

scale' :: (Eq a, Semiring a, G.Vector v a) => Int -> a -> Laurent v a -> Laurent v a
scale' yp yc (Laurent n (Poly xs)) = Laurent n (toPoly' $ scaleInternal zero times yp yc xs)

scaleInternal
  :: (Eq a, G.Vector v a)
  => a
  -> (a -> a -> a)
  -> Int
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

-- | Create an identity polynomial.
pattern X :: (Eq a, Num a, G.Vector v a, Eq (v a)) => Laurent v a
pattern X <- ((==) var -> True)
  where X = var

var :: forall a v. (Eq a, Num a, G.Vector v a, Eq (v a)) => Laurent v a
var
  | (1 :: a) == 0 = Laurent 0 (Poly G.empty)
  | otherwise     = Laurent 0 (Poly $ G.fromList [0, 1])
{-# INLINE var #-}

-- | Evaluate at a given point.
--
-- >>> eval (X^-2 + 1 :: UPoly Rational) 3
-- 10
eval :: (Num a, G.Vector v a, G.Vector v (Int, a), G.Vector v Int, Fractional a) => Laurent v a -> a -> a
eval = substitute (*) recip
{-# INLINE eval #-}

--eval' :: (Semiring a, G.Vector v a) => Laurent v a -> a -> a
--eval' = substitute' times
-- {-# INLINE eval' #-}

data StrictPair a b = !a :*: !b

infixr 1 :*:

fst' :: StrictPair a b -> a
fst' (a :*: _) = a

-- | Substitute another polynomial instead of 'X'.
--
-- >>> subst (X^2 + 1 :: UPoly Int) (X + 1 :: UPoly Int)
-- 1 * X^2 + 2 * X + 2
subst :: (Eq a, Integral a, G.Vector v a, G.Vector v (Int, a), G.Vector v Int) => Laurent v a -> Laurent v a -> Laurent v a
subst = substitute (scale 0) (1 `quotLaurent`)
{-# INLINE subst #-}

substitute :: (Num b, G.Vector v a, G.Vector v (Int, a), G.Vector v Int)
  => (a -> b -> b)
  -> (b -> b)
  -> Laurent v a
  -> b
  -> b
substitute f inv (Laurent n (Poly cs)) x
  | n >= 0 = fst' $
      G.foldl' (\(acc :*: xn) cn -> acc + f cn xn :*: x * xn) (0 :*: 1) cs
  | otherwise = fst' $
      G.foldl' (\(acc :*: xn) (i, cn) -> acc + f cn xn :*: pow x i)
                (0 :*: pow x n)
                (G.zip (G.fromList [(n+1)..(n + G.basicLength cs)]) cs)
  where
    pow b i
      | i < 0 = inv b ^ negate i
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

-- infixr 8 -^
-- (-^) :: (Eq a, Semiring a, G.Vector v (Int, a), Eq (v (Int, a))) => Poly v a -> Int -> Poly v a
-- X' -^ n = monomial' (-n) one
-- _  -^ _ = error "(-^): not the identity Laurent polynomial"
-- {-# INLINE (-^) #-}
