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
module Data.Poly.Laurent
  ( Laurent(..)
  , ULaurent
  , VLaurent
  , leading
--   -- * Num interface
  , toLaurent
  , monomial
  , scale
  , pattern X
  , eval
  -- , subst
  -- , deriv
  -- , integral

   -- * Semiring interface
  , toLaurent'
  , monomial'
  )
where

import           Control.DeepSeq          (NFData)
import           Data.Poly.Internal.Dense (Poly (..), scale, scale', toPoly,
                                           toPoly')
import qualified Data.Poly.Internal.Dense as Poly
import           Data.Semiring            (Semiring (..))
import qualified Data.Vector              as V
import qualified Data.Vector.Generic      as G
import qualified Data.Vector.Unboxed      as U
import           GHC.Generics             (Generic)

-- TODO: Move this to Laurent/Dense.hs

data Laurent v a = Laurent !Int !(Poly v a)
  deriving (Eq, Ord, Generic)
  deriving anyclass (NFData)

type VLaurent a = Laurent V.Vector a
type ULaurent a = Laurent U.Vector a

instance (Eq a, Num a, G.Vector v a) => Num (Laurent v a) where
  Laurent n xs * Laurent m ys = Laurent (n + m) (xs * ys)
  Laurent n xs + Laurent m ys
    | n <= m    = Laurent n (xs + scale (fromIntegral (m - n)) 1 ys)
    | otherwise = Laurent m (scale (fromIntegral (n - m)) 1 xs + ys)
  negate (Laurent n xs) = Laurent n (negate xs)
  abs    = id
  signum = const 1
  fromInteger n = Laurent 0 (fromInteger n)
  {-# INLINE (+) #-}
  {-# INLINE negate #-}
  {-# INLINE fromInteger #-}
  {-# INLINE (*) #-}

-- instance (Show a, G.Vector v (Int, a)) => Show (Laurent v a) where
--   showsPrec d (Poly xs)
--     | G.null xs
--       = showString "0"
--     | otherwise
--       = showParen (d > 0)
--       $ foldl (.) id
--       $ intersperse (showString " + ")
--       $ G.foldl (\acc (i, c) -> showCoeff i c : acc) [] xs
--     where
--       showCoeff 0 c = showsPrec 7 c
--       showCoeff 1 c = showsPrec 7 c . showString " * X"
--       showCoeff i c = showsPrec 7 c . showString " * X^" . showsPrec 6 i

instance (Eq a, Semiring a, G.Vector v a) => Semiring (Laurent v a) where
  zero = Laurent 0 zero
  one  = Laurent 0 one

  Laurent n xs `times` Laurent m ys = Laurent (n + m) (xs `times` ys)
  Laurent n xs `plus` Laurent m ys
    | n <= m    = Laurent n (xs `plus` scale' (fromIntegral (m - n)) one ys)
    | otherwise = Laurent m (scale' (fromIntegral (n - m)) one xs `plus` ys)
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
  | c == zero = Laurent 0 (Poly G.empty)
  | p >= 0     = Laurent 0 (Poly.monomial' (fromIntegral p) c)
  | otherwise  = Laurent p (Poly $ G.singleton c)
{-# INLINE monomial' #-}

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
-- >>> eval (X^2 + 1 :: UPoly Int) 3
-- 10
eval :: (Num a, G.Vector v a) => Laurent v a -> a -> a
eval = substitute (*)
{-# INLINE eval #-}

--eval' :: (Semiring a, G.Vector v a) => Laurent v a -> a -> a
--eval' = substitute' times
-- {-# INLINE eval' #-}

data StrictPair a b = !a :*: !b

infixr 1 :*:

fst' :: StrictPair a b -> a
fst' (a :*: _) = a

-- substitute :: (G.Vector v a, Num b) => (a -> b -> b) -> Laurent v a -> b -> b
-- substitute f (Laurent n (Poly cs)) x = fst' $
--   G.foldl' (\(acc :*: xn) cn -> acc + f cn xn :*: x * xn) (0 :*: 1) cs
-- {-# INLINE substitute #-}

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


