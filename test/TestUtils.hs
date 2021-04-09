{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtils
  ( ShortPoly(..)
  , tenTimesLess
  , myNumLaws
#ifdef MIN_VERSION_quickcheck_classes
  , mySemiringLaws
  , myRingLaws
  , myGcdDomainLaws
  , myEuclideanLaws
#endif
  , myIsListLaws
  , myShowLaws
  ) where

import Prelude hiding (lcm, rem)
import Data.Euclidean
import Data.Mod.Word
import Data.Proxy
import Data.Semiring (Semiring(..), Ring)
import qualified Data.Vector.Generic as G
import GHC.Exts
import GHC.TypeNats (KnownNat)
import Test.QuickCheck.Classes.Base
import Test.Tasty
import Test.Tasty.QuickCheck

#ifdef MIN_VERSION_quickcheck_classes
import Test.QuickCheck.Classes
#endif

import qualified Data.Poly.Semiring as Dense
import qualified Data.Poly.Laurent as DenseLaurent

#ifdef SupportSparse
import Control.Arrow
import Data.Finite
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Unboxed.Sized as SU
import Data.Poly.Multi.Semiring
import qualified Data.Poly.Multi.Laurent as MultiLaurent
#endif

newtype ShortPoly a = ShortPoly { unShortPoly :: a }
  deriving (Eq, Show, Semiring, GcdDomain, Euclidean, Num)

instance KnownNat m => Arbitrary (Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, fromInteger <$> arbitrary]
  shrink = map fromInteger . shrink . toInteger . unMod

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (Dense.Poly v a) where
  arbitrary = Dense.toPoly . G.fromList <$> arbitrary
  shrink = fmap (Dense.toPoly . G.fromList) . shrink . G.toList . Dense.unPoly

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (ShortPoly (Dense.Poly v a)) where
  arbitrary = ShortPoly . Dense.toPoly . G.fromList . (\xs -> take (length xs `mod` 10) xs) <$> arbitrary
  shrink = fmap (ShortPoly . Dense.toPoly . G.fromList) . shrink . G.toList . Dense.unPoly . unShortPoly

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (DenseLaurent.Laurent v a) where
  arbitrary = DenseLaurent.toLaurent <$> ((`rem` 10) <$> arbitrary) <*> arbitrary
  shrink = fmap (uncurry DenseLaurent.toLaurent) . shrink . DenseLaurent.unLaurent

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (ShortPoly (DenseLaurent.Laurent v a)) where
  arbitrary = (ShortPoly .) . DenseLaurent.toLaurent <$> ((`rem` 10) <$> arbitrary) <*> (unShortPoly <$> arbitrary)
  shrink = fmap (ShortPoly . uncurry DenseLaurent.toLaurent . fmap unShortPoly) . shrink . fmap ShortPoly . DenseLaurent.unLaurent . unShortPoly

#ifdef SupportSparse

instance KnownNat n => Arbitrary (Finite n) where
  arbitrary = elements finites

instance (Arbitrary a, KnownNat n, G.Vector v a) => Arbitrary (SG.Vector v n a) where
  arbitrary = SG.replicateM arbitrary
  shrink vs = [ vs SG.// [(i, x)] | i <- finites, x <- shrink (SG.index vs i) ]

instance (Eq a, Semiring a, Arbitrary a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Arbitrary (MultiPoly v n a) where
  arbitrary = toMultiPoly . G.fromList <$> arbitrary
  shrink = fmap (toMultiPoly . G.fromList) . shrink . G.toList . unMultiPoly

instance (Eq a, Semiring a, Arbitrary a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Arbitrary (ShortPoly (MultiPoly v n a)) where
  arbitrary = ShortPoly . toMultiPoly . G.fromList . (\xs -> take (length xs `mod` 4) (map (first (SU.map (`mod` 3))) xs)) <$> arbitrary
  shrink = fmap (ShortPoly . toMultiPoly . G.fromList) . shrink . G.toList . unMultiPoly . unShortPoly

instance (Eq a, Semiring a, Arbitrary a, KnownNat n, G.Vector v (Word, a), G.Vector v (SU.Vector n Word, a)) => Arbitrary (MultiLaurent.MultiLaurent v n a) where
  arbitrary = MultiLaurent.toMultiLaurent <$> (SU.map (`rem` 10) <$> arbitrary) <*> arbitrary
  shrink = fmap (uncurry MultiLaurent.toMultiLaurent) . shrink . MultiLaurent.unMultiLaurent

instance (Eq a, Semiring a, Arbitrary a, KnownNat n, G.Vector v (Word, a), G.Vector v (SU.Vector n Word, a)) => Arbitrary (ShortPoly (MultiLaurent.MultiLaurent v n a)) where
  arbitrary = (ShortPoly .) . MultiLaurent.toMultiLaurent <$> (SU.map (`rem` 10) <$> arbitrary) <*> (unShortPoly <$> arbitrary)
  shrink = fmap (ShortPoly . uncurry MultiLaurent.toMultiLaurent . fmap unShortPoly) . shrink . fmap ShortPoly . MultiLaurent.unMultiLaurent . unShortPoly

#endif

-------------------------------------------------------------------------------

tenTimesLess :: TestTree -> TestTree
tenTimesLess = adjustOption $
  \(QuickCheckTests n) -> QuickCheckTests (max 100 (n `div` 10))

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

#ifdef MIN_VERSION_quickcheck_classes

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

myGcdDomainLaws :: forall a. (Eq a, GcdDomain a, Arbitrary a, Show a) => Proxy a -> TestTree
myGcdDomainLaws proxy = testGroup tpclss $ map tune $ lcm0 : props
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

    lcm0 = ("lcm0", property $ \(x :: a) -> lcm x zero === zero .&&. lcm zero x === zero)

myEuclideanLaws :: (Eq a, Euclidean a, Arbitrary a, Show a) => Proxy a -> TestTree
myEuclideanLaws proxy = testGroup tpclss $ map (uncurry testProperty) props
  where
    Laws tpclss props = euclideanLaws proxy

#endif

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
