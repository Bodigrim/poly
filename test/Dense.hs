{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Dense
  ( testSuite
  , ShortPoly(..)
  ) where

import Prelude hiding (gcd, quotRem, quot, rem)
import Control.Exception
import Data.Euclidean (Euclidean(..), GcdDomain(..))
import Data.Int
import Data.Mod.Word
import Data.Poly
import qualified Data.Poly.Semiring as S
import Data.Proxy
import Data.Semiring (Semiring(..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

import Quaternion
import TestUtils

testSuite :: TestTree
testSuite = testGroup "Dense"
  [ arithmeticTests
  , otherTests
  , divideByZeroTests
  , lawsTests
  , evalTests
  , derivTests
  , patternTests
  , conversionTests
  ]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  $ semiringTests ++ ringTests ++ numTests ++ euclideanTests ++ gcdDomainTests ++ isListTests ++ showTests

semiringTests :: [TestTree]
semiringTests =
  [ mySemiringLaws (Proxy :: Proxy (UPoly ()))
  , mySemiringLaws (Proxy :: Proxy (UPoly Int8))
  , mySemiringLaws (Proxy :: Proxy (VPoly Integer))
  , mySemiringLaws (Proxy :: Proxy (UPoly (Quaternion Int)))
  ]

ringTests :: [TestTree]
ringTests =
  [ myRingLaws (Proxy :: Proxy (UPoly ()))
  , myRingLaws (Proxy :: Proxy (UPoly Int8))
  , myRingLaws (Proxy :: Proxy (VPoly Integer))
  , myRingLaws (Proxy :: Proxy (UPoly (Quaternion Int)))
  ]

numTests :: [TestTree]
numTests =
  [ myNumLaws (Proxy :: Proxy (UPoly Int8))
  , myNumLaws (Proxy :: Proxy (VPoly Integer))
  , myNumLaws (Proxy :: Proxy (UPoly (Quaternion Int)))
  ]

gcdDomainTests :: [TestTree]
gcdDomainTests =
  [ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VPoly Integer)))
  , myGcdDomainLaws (Proxy :: Proxy (ShortPoly (UPoly (Mod 3))))
  , myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VPoly Rational)))
  ]

euclideanTests :: [TestTree]
euclideanTests =
  [ myEuclideanLaws (Proxy :: Proxy (ShortPoly (UPoly (Mod 3))))
  , myEuclideanLaws (Proxy :: Proxy (ShortPoly (VPoly Rational)))
  ]

isListTests :: [TestTree]
isListTests =
  [ myIsListLaws (Proxy :: Proxy (UPoly ()))
  , myIsListLaws (Proxy :: Proxy (UPoly Int8))
  , myIsListLaws (Proxy :: Proxy (VPoly Integer))
  , myIsListLaws (Proxy :: Proxy (UPoly (Quaternion Int)))
  ]

showTests :: [TestTree]
showTests =
  [ myShowLaws (Proxy :: Proxy (UPoly ()))
  , myShowLaws (Proxy :: Proxy (UPoly Int8))
  , myShowLaws (Proxy :: Proxy (VPoly Integer))
  , myShowLaws (Proxy :: Proxy (UPoly (Quaternion Int)))
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
  , tenTimesLess $
    testProperty "scale matches multiplication by monomial" $
    \p c (xs :: UPoly a) -> scale p c xs === monomial p c * xs
  , tenTimesLess $
    testProperty "scale' matches multiplication by monomial'" $
    \p c (xs :: UPoly a) -> S.scale p c xs === S.monomial p c * xs
  ]

monomialRef :: Num a => Word -> a -> [a]
monomialRef p c = replicate (fromIntegral p) 0 ++ [c]

divideByZeroTests :: TestTree
divideByZeroTests = testGroup "divideByZero"
  [ testProperty "quotRem" $ testProp ((uncurry (+) .) . quotRem)
  , testProperty "quot"    $ testProp quot
  , testProperty "rem"     $ testProp rem
  , testProperty "divide"  $ testProp divide
  , testProperty "degree"  $ once $ degree (0 :: VPoly Rational) === 0
  ]
  where
    testProp f xs = ioProperty ((== Left DivideByZero) <$> try (evaluate (xs `f` (0 :: VPoly Rational))))

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup  (Proxy :: Proxy (UPoly Int8))
  , evalTestGroup  (Proxy :: Proxy (VPoly Integer))
  , substTestGroup (Proxy :: Proxy (UPoly Int8))
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

substTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v a), Show (v a), G.Vector v a)
  => Proxy (Poly v a)
  -> [TestTree]
substTestGroup _ =
  [ tenTimesLess $ tenTimesLess $ tenTimesLess $
    testProperty "subst (p + q) r = subst p r + subst q r" $
    \p q r -> e (p + q) r === e p r + e q r
  , testProperty "subst x p = p" $
    \p -> e X p === p
  , testProperty "subst (monomial 0 c) p = monomial 0 c" $
    \c p -> e (monomial 0 c) p === monomial 0 c
  , tenTimesLess $ tenTimesLess $ tenTimesLess $
    testProperty "subst' (p + q) r = subst' p r + subst' q r" $
    \p q r -> e' (p + q) r === e' p r + e' q r
  , testProperty "subst' x p = p" $
    \p -> e' S.X p === p
  , testProperty "subst' (S.monomial 0 c) p = S.monomial 0 c" $
    \c p -> e' (S.monomial 0 c) p === S.monomial 0 c
  ]
  where
    e :: Poly v a -> Poly v a -> Poly v a
    e = subst
    e' :: Poly v a -> Poly v a -> Poly v a
    e' = S.subst

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv = S.deriv" $
    \(p :: VPoly Integer) -> deriv p === S.deriv p
  , testProperty "integral = S.integral" $
    \(p :: VPoly Rational) -> integral p === S.integral p
  , testProperty "deriv . integral = id" $
    \(p :: VPoly Rational) -> deriv (integral p) === p
  , testProperty "deriv c = 0" $
    \c -> deriv (monomial 0 c :: UPoly Int) === 0
  , testProperty "deriv cX = c" $
    \c -> deriv (monomial 0 c * X :: UPoly Int) === monomial 0 c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \p q -> deriv (p + q) === (deriv p + deriv q :: UPoly Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \p q -> deriv (p * q) === (p * deriv q + q * deriv p :: UPoly Int)
  , tenTimesLess $ tenTimesLess $ tenTimesLess $
    testProperty "deriv (subst p q) = deriv q * subst (deriv p) q" $
    \(p :: UPoly Int) (q :: UPoly Int) ->
      deriv (subst p q) === deriv q * subst (deriv p) q
  ]

patternTests :: TestTree
patternTests = testGroup "pattern"
  [ testProperty "X  :: UPoly Int" $ once $
    case (monomial 1 1 :: UPoly Int) of X -> True; _ -> False
  , testProperty "X  :: UPoly Int" $ once $
    (X :: UPoly Int) === monomial 1 1
  , testProperty "X' :: UPoly Int" $ once $
    case (S.monomial 1 1 :: UPoly Int) of S.X -> True; _ -> False
  , testProperty "X' :: UPoly Int" $ once $
    (S.X :: UPoly Int) === S.monomial 1 1
  , testProperty "X' :: UPoly ()" $ once $
    case (zero :: UPoly ()) of S.X -> True; _ -> False
  , testProperty "X' :: UPoly ()" $ once $
    (S.X :: UPoly ()) === zero
  ]

conversionTests :: TestTree
conversionTests = testGroup "conversions"
  [ testProperty "sparseToDense . denseToSparse = id" $
    \(xs :: UPoly Int8) -> xs === sparseToDense (denseToSparse xs)
  , testProperty "sparseToDense' . denseToSparse' = id" $
    \(xs :: UPoly Int8) -> xs === S.sparseToDense (S.denseToSparse xs)
  , testProperty "toPoly . unPoly = id" $
    \(xs :: UPoly Int8) -> xs === toPoly (unPoly xs)
  , testProperty "S.toPoly . S.unPoly = id" $
    \(xs :: UPoly Int8) -> xs === S.toPoly (S.unPoly xs)
  ]
