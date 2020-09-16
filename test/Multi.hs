{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module Multi
  ( testSuite
  ) where

import Prelude hiding (gcd, quotRem, rem)
import Control.Exception
import Data.Euclidean (GcdDomain(..))
import Data.Function
import Data.Int
import Data.List (groupBy, sortOn)
import Data.Mod
import Data.Proxy
import Data.Semiring (Semiring(..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed.Sized as SU
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

import Data.Poly.Multi.Semiring

import Quaternion
import TestUtils

testSuite :: TestTree
testSuite = testGroup "Multi"
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
  $ semiringTests ++ ringTests ++ numTests ++ gcdDomainTests ++ isListTests ++ showTests

semiringTests :: [TestTree]
semiringTests =
  [ mySemiringLaws (Proxy :: Proxy (VMultiPoly 3 ()))
  , mySemiringLaws (Proxy :: Proxy (VMultiPoly 3 Int8))
  , mySemiringLaws (Proxy :: Proxy (VMultiPoly 3 Integer))
  , tenTimesLess
  $ mySemiringLaws (Proxy :: Proxy (VMultiPoly 3 (Quaternion Int)))
  ]

ringTests :: [TestTree]
ringTests =
  [ myRingLaws (Proxy :: Proxy (VMultiPoly 3 ()))
  , myRingLaws (Proxy :: Proxy (VMultiPoly 3 Int8))
  , myRingLaws (Proxy :: Proxy (VMultiPoly 3 Integer))
  , myRingLaws (Proxy :: Proxy (VMultiPoly 3 (Quaternion Int)))
  ]

numTests :: [TestTree]
numTests =
  [ myNumLaws (Proxy :: Proxy (VMultiPoly 3 Int8))
  , myNumLaws (Proxy :: Proxy (VMultiPoly 3 Integer))
  , tenTimesLess
  $ myNumLaws (Proxy :: Proxy (VMultiPoly 3 (Quaternion Int)))
  ]

gcdDomainTests :: [TestTree]
gcdDomainTests =
  [ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VMultiPoly 3 Integer)))
  , tenTimesLess
  $ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VMultiPoly 3 (Mod 3))))
  , tenTimesLess
  $ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VMultiPoly 3 Rational)))
  ]

isListTests :: [TestTree]
isListTests =
  [ myIsListLaws (Proxy :: Proxy (VMultiPoly 3 ()))
  , myIsListLaws (Proxy :: Proxy (VMultiPoly 3 Int8))
  , myIsListLaws (Proxy :: Proxy (VMultiPoly 3 Integer))
  , tenTimesLess
  $ myIsListLaws (Proxy :: Proxy (VMultiPoly 3 (Quaternion Int)))
  ]

showTests :: [TestTree]
showTests =
  [ myShowLaws (Proxy :: Proxy (VMultiPoly 4 ()))
  , myShowLaws (Proxy :: Proxy (VMultiPoly 4 Int8))
  , myShowLaws (Proxy :: Proxy (VMultiPoly 4 Integer))
  , tenTimesLess
  $ myShowLaws (Proxy :: Proxy (VMultiPoly 4 (Quaternion Int)))
  ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic"
  [ testProperty "addition matches reference" $
    \(xs :: [(SU.Vector 3 Word, Int)]) ys -> toMultiPoly (V.fromList (addRef xs ys)) ===
      toMultiPoly (V.fromList xs) + toMultiPoly (V.fromList ys)
  , testProperty "subtraction matches reference" $
    \(xs :: [(SU.Vector 3 Word, Int)]) ys -> toMultiPoly (V.fromList (subRef xs ys)) ===
      toMultiPoly (V.fromList xs) - toMultiPoly (V.fromList ys)
  , tenTimesLess $
    testProperty "multiplication matches reference" $
    \(xs :: [(SU.Vector 3 Word, Int)]) ys -> toMultiPoly (V.fromList (mulRef xs ys)) ===
      toMultiPoly (V.fromList xs) * toMultiPoly (V.fromList ys)
  ]

addRef :: (Num a, Ord t) => [(t, a)] -> [(t, a)] -> [(t, a)]
addRef [] ys = ys
addRef xs [] = xs
addRef xs@((xp, xc) : xs') ys@((yp, yc) : ys') =
  case xp `compare` yp of
    LT -> (xp, xc) : addRef xs' ys
    EQ -> (xp, xc + yc) : addRef xs' ys'
    GT -> (yp, yc) : addRef xs ys'

subRef :: (Num a, Ord t) => [(t, a)] -> [(t, a)] -> [(t, a)]
subRef [] ys = map (fmap negate) ys
subRef xs [] = xs
subRef xs@((xp, xc) : xs') ys@((yp, yc) : ys') =
  case xp `compare` yp of
    LT -> (xp, xc) : subRef xs' ys
    EQ -> (xp, xc - yc) : subRef xs' ys'
    GT -> (yp, negate yc) : subRef xs ys'

mulRef :: (Num a, Ord t, Num t) => [(t, a)] -> [(t, a)] -> [(t, a)]
mulRef xs ys
  = map (\ws -> (fst (head ws), sum (map snd ws)))
  $ groupBy ((==) `on` fst)
  $ sortOn fst
  $ [ (xp + yp, xc * yc) | (xp, xc) <- xs, (yp, yc) <- ys ]

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
  [ testProperty "monomial matches reference" $
    \(ps :: SU.Vector 3 Word) (c :: a) -> monomial ps c === toMultiPoly (V.fromList (monomialRef ps c))
  , tenTimesLess $
    testProperty "scale matches multiplication by monomial" $
    \ps c (xs :: VMultiPoly 3 a) -> scale ps c xs === monomial ps c * xs
  ]

monomialRef :: Num a => t -> a -> [(t, a)]
monomialRef p c = [(p, c)]

divideByZeroTests :: TestTree
divideByZeroTests = testGroup "divideByZero"
  [ testProperty "divide"  $ testProp divide
  ]
  where
    testProp f xs = ioProperty ((== Left DivideByZero) <$> try (evaluate (xs `f` (0 :: VMultiPoly 3 Rational))))

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup  (Proxy :: Proxy (VMultiPoly 3 Int8))
  , evalTestGroup  (Proxy :: Proxy (VMultiPoly 3 Integer))
  , substTestGroup (Proxy :: Proxy (VMultiPoly 3 Int8))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v (SU.Vector 3 Word, a)), Show (v (SU.Vector 3 Word, a)), G.Vector v (SU.Vector 3 Word, a))
  => Proxy (MultiPoly v 3 a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) rs = eval p rs + eval q rs" $
    \p q rs -> e (p + q) rs === e p rs + e q rs
  , testProperty "eval (p * q) rs = eval p rs * eval q rs" $
    \p q rs -> e (p * q) rs === e p rs * e q rs
  , testProperty "eval x p = p" $
    \p -> e X (SV.fromTuple (p, undefined, undefined)) === p
  , testProperty "eval (monomial 0 c) p = c" $
    \c ps -> e (monomial 0 c) ps === c
  ]
  where
    e :: MultiPoly v 3 a -> SV.Vector 3 a -> a
    e = eval

substTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v (SU.Vector 3 Word, a)), Show (v (SU.Vector 3 Word, a)), G.Vector v (SU.Vector 3 Word, a))
  => Proxy (MultiPoly v 3 a)
  -> [TestTree]
substTestGroup _ =
  [ testProperty "subst x p = p" $
    \p -> e X (SV.fromTuple (p, undefined, undefined)) === p
  , testProperty "subst (monomial 0 c) ps = monomial 0 c" $
    \c ps -> e (monomial 0 c) ps === monomial 0 c
  ]
  where
    e :: MultiPoly v 3 a -> SV.Vector 3 (MultiPoly v 3 a) -> MultiPoly v 3 a
    e = subst

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv . integral = id" $
    \k (p :: VMultiPoly 3 Rational) ->
      deriv k (integral k p) === p
  , testProperty "deriv c = 0" $
    \k c ->
      deriv k (monomial 0 c :: VMultiPoly 3 Int) === 0
  , testProperty "deriv cX = c" $
    \c ->
      deriv 0 (monomial 0 c * X :: VMultiPoly 3 Int) === monomial 0 c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \k p q ->
      deriv k (p + q) === (deriv k p + deriv k q :: VMultiPoly 3 Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \k p q ->
      deriv k (p * q) === (p * deriv k q + q * deriv k p :: VMultiPoly 3 Int)
  -- , testProperty "deriv (subst p q) = deriv q * subst (deriv p) q" $
  --   \(p :: Poly V.Vector Int) (q :: Poly U.Vector Int) k ->
  --     deriv k (subst p q) === deriv k q * subst (deriv k p) q
  ]

patternTests :: TestTree
patternTests = testGroup "pattern"
  [ testProperty "X  :: UMultiPoly Int" $ once $
    case (monomial 1 1 :: UMultiPoly 1 Int) of X -> True; _ -> False
  , testProperty "X  :: UMultiPoly Int" $ once $
    (X :: UMultiPoly 1 Int) === monomial 1 1
  , testProperty "X :: UMultiPoly ()" $ once $
    case (zero :: UMultiPoly 1 ()) of X -> True; _ -> False
  , testProperty "X :: UMultiPoly ()" $ once $
    (X :: UMultiPoly 1 ()) === zero

  , testProperty "Y  :: UMultiPoly Int" $ once $
    case (monomial (SG.fromTuple (0, 1)) 1 :: UMultiPoly 2 Int) of Y -> True; _ -> False
  , testProperty "Y  :: UMultiPoly Int" $ once $
    (Y :: UMultiPoly 2 Int) === monomial (SG.fromTuple (0, 1)) 1
  , testProperty "Y :: UMultiPoly ()" $ once $
    case (zero :: UMultiPoly 2 ()) of Y -> True; _ -> False
  , testProperty "Y :: UMultiPoly ()" $ once $
    (Y :: UMultiPoly 2 ()) === zero

  , testProperty "Z  :: UMultiPoly Int" $ once $
    case (monomial (SG.fromTuple (0, 0, 1)) 1 :: UMultiPoly 3 Int) of Z -> True; _ -> False
  , testProperty "Z  :: UMultiPoly Int" $ once $
    (Z :: UMultiPoly 3 Int) === monomial (SG.fromTuple (0, 0, 1)) 1
  , testProperty "Z :: UMultiPoly ()" $ once $
    case (zero :: UMultiPoly 3 ()) of Z -> True; _ -> False
  , testProperty "Z :: UMultiPoly ()" $ once $
    (Z :: UMultiPoly 3 ()) === zero
  ]

conversionTests :: TestTree
conversionTests = testGroup "conversions"
  [ testProperty "unsegregate . segregate = id" $
    \(xs :: UMultiPoly 3 Int8) -> xs === unsegregate (segregate xs)
  , testProperty "segregate . unsegregate = id" $
    \xs -> xs === segregate (unsegregate xs :: UMultiPoly 3 Int8)
  ]
