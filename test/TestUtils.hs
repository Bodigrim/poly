{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}

module TestUtils
  ( tenTimesLess
  , mySemiringLaws
#if MIN_VERSION_quickcheck_classes(0,6,1)
  , myRingLaws
#endif
#if MIN_VERSION_quickcheck_classes(0,6,3)
  , myNumLaws
#endif
#if MIN_VERSION_semirings(0,4,2) && MIN_VERSION_quickcheck_classes(0,6,3)
  , myGcdDomainLaws
  , myEuclideanLaws
#endif
  , myIsListLaws
#if MIN_VERSION_quickcheck_classes(0,6,0)
  , myShowLaws
#endif
  ) where

#if MIN_VERSION_semirings(0,4,2)
import Data.Euclidean
#endif
import Data.Proxy
import Data.Semiring
import GHC.Exts
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

tenTimesLess :: TestTree -> TestTree
tenTimesLess = adjustOption $
  \(QuickCheckTests n) -> QuickCheckTests (max 100 (n `div` 10))

mySemiringLaws :: (Eq a, Semiring a, Arbitrary a, Show a) => Proxy a -> TestTree
mySemiringLaws proxy = testGroup tpclss $ map tune props
  where
    Laws tpclss props = semiringLaws proxy

    tune pair = case fst pair of
      "Multiplicative Associativity" ->
        tenTimesLess test
      "Multiplication Left Distributes Over Addition" ->
        tenTimesLess test
      "Multiplication Right Distributes Over Addition" ->
        tenTimesLess test
      _ -> test
      where
        test = uncurry testProperty pair

#if MIN_VERSION_quickcheck_classes(0,6,1)
myRingLaws :: (Eq a, Ring a, Arbitrary a, Show a) => Proxy a -> TestTree
myRingLaws proxy = testGroup tpclss $ map (uncurry testProperty) props
  where
    Laws tpclss props = ringLaws proxy
#endif

#if MIN_VERSION_quickcheck_classes(0,6,3)
myNumLaws :: (Eq a, Num a, Arbitrary a, Show a) => Proxy a -> TestTree
myNumLaws proxy = testGroup tpclss $ map tune props
  where
    Laws tpclss props = numLaws proxy

    tune pair = case fst pair of
      "Multiplicative Associativity" ->
        tenTimesLess test
      "Multiplication Left Distributes Over Addition" ->
        tenTimesLess test
      "Multiplication Right Distributes Over Addition" ->
        tenTimesLess test
      "Subtraction" ->
        tenTimesLess test
      _ -> test
      where
        test = uncurry testProperty pair
#endif

#if MIN_VERSION_semirings(0,4,2) && MIN_VERSION_quickcheck_classes(0,6,3)
myGcdDomainLaws :: (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> TestTree
myGcdDomainLaws proxy = testGroup tpclss $ map tune props
  where
    Laws tpclss props = gcdDomainLaws proxy

    tune pair = case fst pair of
      "gcd1"    -> tenTimesLess $ tenTimesLess test
      "gcd2"    -> tenTimesLess $ tenTimesLess test
      "lcm1"    -> tenTimesLess $ tenTimesLess $ tenTimesLess test
      "lcm2"    -> tenTimesLess test
      "coprime" -> tenTimesLess $ tenTimesLess test
      _ -> test
      where
        test = uncurry testProperty pair

myEuclideanLaws :: (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> TestTree
myEuclideanLaws proxy = testGroup tpclss $ map (uncurry testProperty) props
  where
    Laws tpclss props = euclideanLaws proxy
#endif

myIsListLaws :: (Eq a, IsList a, Arbitrary a, Show a, Show (Item a), Arbitrary (Item a)) => Proxy a -> TestTree
myIsListLaws proxy = testGroup tpclss $ map (uncurry testProperty) props
  where
    Laws tpclss props = isListLaws proxy

#if MIN_VERSION_quickcheck_classes(0,6,0)
myShowLaws :: (Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
myShowLaws proxy = testGroup tpclss $ map tune props
  where
    Laws tpclss props = showLaws proxy

    tune pair = case fst pair of
      "Equivariance: showList" -> tenTimesLess $ tenTimesLess test
      _ -> test
      where
        test = uncurry testProperty pair
#endif
