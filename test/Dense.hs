{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dense
  ( testSuite
  ) where

import Prelude hiding (gcd, quotRem, rem)
#if MIN_VERSION_semirings(0,4,2)
import Data.Euclidean
#endif
import Data.Int
import Data.Maybe
import Data.Poly
import qualified Data.Poly.Semiring as S
import Data.Proxy
import Data.Semiring (Semiring)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)
import Test.QuickCheck.Classes

import Quaternion

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (Poly v a) where
  arbitrary = S.toPoly . G.fromList <$> arbitrary
  shrink = fmap (S.toPoly . G.fromList) . shrink . G.toList . unPoly

#if MIN_VERSION_semirings(0,4,2)
instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (PolyOverField (Poly v a)) where
  arbitrary = PolyOverField . S.toPoly . G.fromList . (\xs -> take (length xs `mod` 10) xs) <$> arbitrary
  shrink = fmap (PolyOverField . S.toPoly . G.fromList) . shrink . G.toList . unPoly . unPolyOverField
#endif

newtype ShortPoly a = ShortPoly { unShortPoly :: a }
  deriving
    ( Eq
    , Show
    , Semiring
#if MIN_VERSION_semirings(0,4,2)
    , GcdDomain
    , Euclidean
#endif
    )

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (ShortPoly (Poly v a)) where
  arbitrary = ShortPoly . S.toPoly . G.fromList . (\xs -> take (length xs `mod` 10) xs) <$> arbitrary
  shrink = fmap (ShortPoly . S.toPoly . G.fromList) . shrink . G.toList . unPoly . unShortPoly

testSuite :: TestTree
testSuite = testGroup "Dense"
    [ arithmeticTests
    , otherTests
    , lawsTests
    , evalTests
    , derivTests
#if MIN_VERSION_semirings(0,4,2)
    , gcdExtTests
#endif
    ]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  [ semiringTests
  , ringTests
  , numTests
  , euclideanTests
  , isListTests
  , showTests
  ]

semiringTests :: TestTree
semiringTests
  = testGroup "Semiring"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ semiringLaws (Proxy :: Proxy (Poly U.Vector ()))
  , semiringLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , semiringLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , semiringLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
  ]

ringTests :: TestTree
ringTests
  = testGroup "Ring"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_quickcheck_classes(0,6,1)
    ringLaws (Proxy :: Proxy (Poly U.Vector ()))
  , ringLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , ringLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , ringLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
#endif
  ]

numTests :: TestTree
numTests
  = testGroup "Num"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_quickcheck_classes(0,6,3)
    numLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , numLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , numLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
#endif
  ]

euclideanTests :: TestTree
euclideanTests
  = testGroup "Euclidean"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_semirings(0,4,2) && MIN_VERSION_quickcheck_classes(0,6,3)
    gcdDomainLaws (Proxy :: Proxy (ShortPoly (Poly V.Vector Integer)))
  , gcdDomainLaws (Proxy :: Proxy (PolyOverField (Poly V.Vector Rational)))
  , euclideanLaws (Proxy :: Proxy (ShortPoly (Poly V.Vector Rational)))
#endif
  ]

isListTests :: TestTree
isListTests
  = testGroup "IsList"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ isListLaws (Proxy :: Proxy (Poly U.Vector ()))
  , isListLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , isListLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , isListLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
  ]

showTests :: TestTree
showTests
  = testGroup "Show"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [
#if MIN_VERSION_quickcheck_classes(0,6,0)
    showLaws (Proxy :: Proxy (Poly U.Vector ()))
  , showLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , showLaws (Proxy :: Proxy (Poly V.Vector Integer))
  , showLaws (Proxy :: Proxy (Poly U.Vector (Quaternion Int)))
#endif
  ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic"
  [ testProperty "addition matches reference" $
    \(xs :: [Int]) ys -> toPoly (V.fromList (addRef xs ys)) ===
      toPoly (V.fromList xs) + toPoly (V.fromList ys)
  , testProperty "subtraction matches reference" $
    \(xs :: [Int]) ys -> toPoly (V.fromList (subRef xs ys)) ===
      toPoly (V.fromList xs) - toPoly (V.fromList ys)
  , testProperty "multiplication matches reference" $
    \(xs :: [Int]) ys -> toPoly (V.fromList (mulRef xs ys)) ===
      toPoly (V.fromList xs) * toPoly (V.fromList ys)
  ]

addRef :: Num a => [a] -> [a] -> [a]
addRef [] ys = ys
addRef xs [] = xs
addRef (x : xs) (y : ys) = (x + y) : addRef xs ys

subRef :: Num a => [a] -> [a] -> [a]
subRef [] ys = map negate ys
subRef xs [] = xs
subRef (x : xs) (y : ys) = (x - y) : subRef xs ys

mulRef :: Num a => [a] -> [a] -> [a]
mulRef xs ys
  = foldl addRef []
  $ zipWith (\x zs -> map (* x) zs) xs
  $ iterate (0 :) ys

otherTests :: TestTree
otherTests = testGroup "other" $ concat
  [ otherTestGroup (Proxy :: Proxy Int8)
  , otherTestGroup (Proxy :: Proxy (Quaternion Int))
  ]

otherTestGroup
  :: forall a.
     (Eq a, Show a, Semiring a, Num a, Arbitrary a, U.Unbox a, G.Vector U.Vector a)
  => Proxy a
  -> [TestTree]
otherTestGroup _ =
  [ testProperty "leading p 0 == Nothing" $
    \p -> leading (monomial p 0 :: UPoly a) === Nothing
  , testProperty "leading . monomial = id" $
    \p c -> c /= 0 ==> leading (monomial p c :: UPoly a) === Just (p, c)
  , testProperty "monomial matches reference" $
    \p (c :: a) -> monomial p c === toPoly (V.fromList (monomialRef p c))
  , testProperty "scale matches multiplication by monomial" $
    \p c (xs :: UPoly a) -> scale p c xs === monomial p c * xs
  ]

monomialRef :: Num a => Word -> a -> [a]
monomialRef p c = replicate (fromIntegral p) 0 ++ [c]

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup (Proxy :: Proxy (Poly U.Vector Int8))
  , evalTestGroup (Proxy :: Proxy (Poly V.Vector Integer))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v a), Show (v a), G.Vector v a)
  => Proxy (Poly v a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) r = eval p r + eval q r" $
    \p q r -> e (p + q) r === e p r + e q r
  , testProperty "eval (p * q) r = eval p r * eval q r" $
    \p q r -> e (p * q) r === e p r * e q r
  , testProperty "eval x p = p" $
    \p -> e X p === p
  , testProperty "eval (monomial 0 c) p = c" $
    \c p -> e (monomial 0 c) p === c

  , testProperty "eval' (p + q) r = eval' p r + eval' q r" $
    \p q r -> e' (p + q) r === e' p r + e' q r
  , testProperty "eval' (p * q) r = eval' p r * eval' q r" $
    \p q r -> e' (p * q) r === e' p r * e' q r
  , testProperty "eval' x p = p" $
    \p -> e' S.X p === p
  , testProperty "eval' (S.monomial 0 c) p = c" $
    \c p -> e' (S.monomial 0 c) p === c
  ]

  where
    e :: Poly v a -> a -> a
    e = eval
    e' :: Poly v a -> a -> a
    e' = S.eval

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv = S.deriv" $
    \(p :: Poly V.Vector Integer) -> deriv p === S.deriv p
  , testProperty "integral = S.integral" $
    \(p :: Poly V.Vector Rational) -> integral p === S.integral p
  , testProperty "deriv . integral = id" $
    \(p :: Poly V.Vector Rational) -> deriv (integral p) === p
  , testProperty "deriv c = 0" $
    \c -> deriv (monomial 0 c :: Poly V.Vector Int) === 0
  , testProperty "deriv cX = c" $
    \c -> deriv (monomial 0 c * X :: Poly V.Vector Int) === monomial 0 c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \p q -> deriv (p + q) === (deriv p + deriv q :: Poly V.Vector Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \p q -> deriv (p * q) === (p * deriv q + q * deriv p :: Poly V.Vector Int)
  -- The following property takes too long for a regular test-suite
  -- , testProperty "deriv (eval p q) = deriv q * eval (deriv p) q" $
  --   \(p :: Poly V.Vector Int) (q :: Poly U.Vector Int) ->
  --     deriv (eval (toPoly $ fmap (monomial 0) $ unPoly p) q) ===
  --       deriv q * eval (toPoly $ fmap (monomial 0) $ unPoly $ deriv p) q
  ]

#if MIN_VERSION_semirings(0,4,2)
gcdExtTests :: TestTree
gcdExtTests = localOption (QuickCheckMaxSize 12) $ testGroup "gcdExt"
  [ testProperty "gcdExt == S.gcdExt" $
    \(a :: Poly V.Vector Rational) b ->
      gcdExt a b === S.gcdExt a b
  , testProperty "g == as (mod b) for gcdExt" $
    \(a :: Poly V.Vector Rational) b -> b /= 0 ==>
      uncurry ((. flip rem b) . (===) . flip rem b) ((* a) <$> gcdExt a b)
  , testProperty "fst . gcdExt == gcd (mod units)" $
    \(a :: Poly V.Vector Rational) b ->
      fst (gcdExt a b) `sameUpToUnits` gcd a b
  ]

sameUpToUnits :: (Eq a, GcdDomain a) => a -> a -> Bool
sameUpToUnits x y = x == y ||
  isJust (x `divide` y) && isJust (y `divide` x)
#endif
