-- |
-- Module:      Data.Poly.Internal.Multi.GcdDomain
-- Copyright:   (c) 2019 Andrew Lelechenko
-- Licence:     BSD3
-- Maintainer:  Andrew Lelechenko <andrew.lelechenko@gmail.com>
--
-- GcdDomain for GcdDomain underlying
--

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints      #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Poly.Internal.Multi.GcdDomain
  () where

import Prelude hiding (gcd, lcm, (^))
import Control.Exception
import Data.Euclidean
import Data.Maybe
import Data.Proxy
import Data.Semiring (Semiring(..), Ring(), minus)
import Data.Type.Equality
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.TypeNats (KnownNat, type (+), SomeNat(..), natVal, sameNat, someNatVal)
import Unsafe.Coerce

import Data.Poly.Internal.Multi

#if __GLASGOW_HASKELL__ < 806
import qualified Data.Vector as V
#endif

instance {-# OVERLAPPING #-} (Eq a, Ring a, GcdDomain a, G.Vector v (SU.Vector 1 Word, a)) => GcdDomain (Poly v a) where
  divide xs ys
    | G.null (unMultiPoly ys) = throw DivideByZero
    | G.length (unMultiPoly ys) == 1 = divideSingleton xs (G.unsafeHead (unMultiPoly ys))
    | otherwise = divide1 xs ys

  gcd xs ys
    | G.null (unMultiPoly xs) = ys
    | G.null (unMultiPoly ys) = xs
    | G.length (unMultiPoly xs) == 1 = gcdSingleton (G.unsafeHead (unMultiPoly xs)) ys
    | G.length (unMultiPoly ys) == 1 = gcdSingleton (G.unsafeHead (unMultiPoly ys)) xs
    | otherwise = gcd1 xs ys

  lcm xs ys
    | G.null (unMultiPoly xs) || G.null (unMultiPoly ys) = zero
    | otherwise = (xs `divide'` gcd xs ys) `times` ys

  coprime x y = isJust (one `divide` gcd x y)

data IsSucc n where
  IsSucc :: KnownNat m => n :~: 1 + m -> IsSucc n

-- | This is unsafe when n ~ 0.
isSucc :: forall n. KnownNat n => IsSucc n
isSucc = case someNatVal (natVal (Proxy :: Proxy n) - 1) of
  SomeNat (_ :: Proxy m) -> IsSucc (unsafeCoerce Refl :: n :~: 1 + m)

#if __GLASGOW_HASKELL__ >= 806
instance (Eq a, Ring a, GcdDomain a, KnownNat n, forall m. KnownNat m => G.Vector v (SU.Vector m Word, a), forall m. KnownNat m => Eq (v (SU.Vector m Word, a))) => GcdDomain (MultiPoly v n a) where
#else
instance (Eq a, Ring a, GcdDomain a, KnownNat n, v ~ V.Vector) => GcdDomain (MultiPoly v n a) where
#endif
  divide xs ys
    | G.null (unMultiPoly ys) = throw DivideByZero
    | G.length (unMultiPoly ys) == 1 = divideSingleton xs (G.unsafeHead (unMultiPoly ys))
    -- Polynomials of zero variables are necessarily constants,
    -- so they have been dealt with above.
    | Just Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1)
    = divide1 xs ys
    | otherwise = case isSucc :: IsSucc n of
      IsSucc Refl -> unsegregate <$> segregate xs `divide` segregate ys
  gcd xs ys
    | G.null (unMultiPoly xs) = ys
    | G.null (unMultiPoly ys) = xs
    | G.length (unMultiPoly xs) == 1 = gcdSingleton (G.unsafeHead (unMultiPoly xs)) ys
    | G.length (unMultiPoly ys) == 1 = gcdSingleton (G.unsafeHead (unMultiPoly ys)) xs
    -- Polynomials of zero variables are necessarily constants,
    -- so they have been dealt with above.
    | Just Refl <- sameNat (Proxy :: Proxy n) (Proxy :: Proxy 1)
    = gcd1 xs ys
    | otherwise = case isSucc :: IsSucc n of
      IsSucc Refl -> unsegregate $ segregate xs `gcd` segregate ys

divideSingleton
  :: (GcdDomain a, G.Vector v (SU.Vector n Word, a))
  => MultiPoly v n a
  -> (SU.Vector n Word, a)
  -> Maybe (MultiPoly v n a)
divideSingleton (MultiPoly pcs) (p, c) = MultiPoly <$> G.mapM divideMonomial pcs
  where
    divideMonomial (p', c')
      | SU.and (SU.zipWith (>=) p' p)
      , Just c'' <- c' `divide` c
      = Just (SU.zipWith (-) p' p, c'')
      | otherwise
      = Nothing

gcdSingleton
  :: (Eq a, GcdDomain a, G.Vector v (SU.Vector n Word, a))
  => (SU.Vector n Word, a)
  -> MultiPoly v n a
  -> MultiPoly v n a
gcdSingleton pc (MultiPoly pcs) = uncurry monomial' $
  G.foldl' (\(accP, accC) (p, c) -> (SU.zipWith min accP p, gcd accC c)) pc pcs

divide1
  :: (Eq a, GcdDomain a, Ring a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> Poly v a
  -> Maybe (Poly v a)
divide1 xs ys = case leading ys of
  Nothing -> throw DivideByZero
  Just (yp, yc) -> case leading xs of
    Nothing -> Just xs
    Just (xp, xc)
      | xp < yp -> Nothing
      | otherwise -> do
        zc <- divide xc yc
        let z = MultiPoly $ G.singleton (SU.singleton (xp - yp), zc)
        rest <- divide1 (xs `minus` z `times` ys) ys
        pure $ rest `plus` z

gcd1
  :: (Eq a, GcdDomain a, Ring a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
gcd1 x@(MultiPoly xs) y@(MultiPoly ys) =
  times xy (divide1' z (monomial' 0 (content zs)))
  where
    z@(MultiPoly zs) = gcdHelper x y
    xy = monomial' 0 (gcd (content xs) (content ys))
    divide1' = (fromMaybe (error "gcd: violated internal invariant") .) . divide1

content :: (GcdDomain a, G.Vector v (t, a)) => v (t, a) -> a
content = G.foldl' (\acc (_, t) -> gcd acc t) zero

gcdHelper
  :: (Eq a, Ring a, GcdDomain a, G.Vector v (SU.Vector 1 Word, a))
  => Poly v a
  -> Poly v a
  -> Poly v a
gcdHelper xs ys = case (leading xs, leading ys) of
  (Nothing, _) -> ys
  (_, Nothing) -> xs
  (Just (xp, xc), Just (yp, yc))
    | yp <= xp
    , Just xy <- xc `divide` yc
    -> gcdHelper ys (xs `minus` ys `times` monomial' (SU.singleton (xp - yp)) xy)
    | xp <= yp
    , Just yx <- yc `divide` xc
    -> gcdHelper xs (ys `minus` xs `times` monomial' (SU.singleton (yp - xp)) yx)
    | yp <= xp
    -> gcdHelper ys (xs `times` monomial' 0 gx `minus` ys `times` monomial' (SU.singleton (xp - yp)) gy)
    | otherwise
    -> gcdHelper xs (ys `times` monomial' 0 gy `minus` xs `times` monomial' (SU.singleton (yp - xp)) gx)
    where
      g = lcm xc yc
      gx = divide' g xc
      gy = divide' g yc

divide' :: GcdDomain a => a -> a -> a
divide' = (fromMaybe (error "gcd: violated internal invariant") .) . divide
