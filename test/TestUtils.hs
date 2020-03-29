{-# LANGUAGE FlexibleContexts #-}

module TestUtils
  ( tenTimesLess
  , mySemiringLaws
  , myRingLaws
  , myNumLaws
  , myGcdDomainLaws
  , myEuclideanLaws
  , myIsListLaws
  , myShowLaws
  ) where

import Data.Euclidean
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

myRingLaws :: (Eq a, Ring a, Arbitrary a, Show a) => Proxy a -> TestTree
myRingLaws proxy = testGroup tpclss $ map (uncurry testProperty) props
  where
    Laws tpclss props = ringLaws proxy

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

myIsListLaws :: (Eq a, IsList a, Arbitrary a, Show a, Show (Item a), Arbitrary (Item a)) => Proxy a -> TestTree
myIsListLaws proxy = testGroup tpclss $ map (uncurry testProperty) props
  where
    Laws tpclss props = isListLaws proxy

myShowLaws :: (Eq a, Arbitrary a, Show a) => Proxy a -> TestTree
myShowLaws proxy = testGroup tpclss $ map tune props
  where
    Laws tpclss props = showLaws proxy

    tune pair = case fst pair of
      "Equivariance: showList" -> tenTimesLess $ tenTimesLess test
      _ -> test
      where
        test = uncurry testProperty pair
