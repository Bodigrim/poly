{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Multi
  ( testSuite
  ) where

import Prelude hiding (gcd, quotRem, rem)
import Control.Arrow
import Data.Euclidean (GcdDomain)
import Data.Finite
import Data.Function
import Data.Int
import Data.List (groupBy, sortOn)
import Data.Mod
import Data.Poly.Sparse.Multi
import Data.Proxy
import Data.Semiring (Semiring)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed.Sized as SU
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (KnownNat)
#else
import GHC.TypeLits (KnownNat)
#endif

import Quaternion
import TestUtils

instance KnownNat n => Arbitrary (Finite n) where
  arbitrary = elements finites

instance (Arbitrary a, G.Vector v a) => Arbitrary (SG.Vector v 3 a) where
  arbitrary = SG.fromTuple <$> arbitrary
  shrink vs = SG.fromTuple <$> shrink (SG.index vs 0, SG.index vs 1, SG.index vs 2)

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (SU.Vector 3 Word, a)) => Arbitrary (MultiPoly v 3 a) where
  arbitrary = toMultiPoly . G.fromList <$> arbitrary
  shrink = fmap (toMultiPoly . G.fromList) . shrink . G.toList . unMultiPoly

newtype ShortMultiPoly a = ShortMultiPoly { unShortMultiPoly :: a }
  deriving (Eq, Show, Semiring, GcdDomain)

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (SU.Vector 3 Word, a)) => Arbitrary (ShortMultiPoly (MultiPoly v 3 a)) where
  arbitrary = ShortMultiPoly . toMultiPoly . G.fromList . (\xs -> take (length xs `mod` 4) (map (first (SU.map (`mod` 3))) xs)) <$> arbitrary
  shrink = fmap (ShortMultiPoly . toMultiPoly . G.fromList) . shrink . G.toList . unMultiPoly . unShortMultiPoly

testSuite :: TestTree
testSuite = testGroup "Multi"
  [ arithmeticTests
  , otherTests
  , lawsTests
  , evalTests
  , derivTests
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
  [ myGcdDomainLaws (Proxy :: Proxy (ShortMultiPoly (VMultiPoly 3 Integer)))
  , tenTimesLess
  $ myGcdDomainLaws (Proxy :: Proxy (ShortMultiPoly (VMultiPoly 3 (Mod 3))))
  , tenTimesLess
  $ myGcdDomainLaws (Proxy :: Proxy (ShortMultiPoly (VMultiPoly 3 Rational)))
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
  [ myShowLaws (Proxy :: Proxy (VMultiPoly 3 ()))
  , myShowLaws (Proxy :: Proxy (VMultiPoly 3 Int8))
  , myShowLaws (Proxy :: Proxy (VMultiPoly 3 Integer))
  , tenTimesLess
  $ myShowLaws (Proxy :: Proxy (VMultiPoly 3 (Quaternion Int)))
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
