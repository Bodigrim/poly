{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestUtils
  ( ShortPoly(..)
  , tenTimesLess
  , mySemiringLaws
  , myRingLaws
  , myNumLaws
  , myGcdDomainLaws
  , myEuclideanLaws
  , myIsListLaws
  , myShowLaws
  ) where

import Control.Arrow
import Data.Euclidean hiding (rem)
import Data.Finite
import Data.Mod
import Data.Proxy
import Data.Semiring (Semiring, Ring)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Unboxed.Sized as SU
import GHC.Exts
import Test.QuickCheck.Classes
import Test.Tasty
import Test.Tasty.QuickCheck

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (KnownNat)
#else
import GHC.TypeLits (KnownNat)
#endif

import qualified Data.Poly.Semiring as Dense
import qualified Data.Poly.Laurent as DenseLaurent
import Data.Poly.Multi.Semiring
import qualified Data.Poly.Sparse.Laurent as SparseLaurent

newtype ShortPoly a = ShortPoly { unShortPoly :: a }
  deriving (Eq, Show, Semiring, GcdDomain, Euclidean)

instance KnownNat m => Arbitrary (Mod m) where
  arbitrary = oneof [arbitraryBoundedEnum, fromInteger <$> arbitrary]
  shrink = map fromInteger . shrink . toInteger . unMod

instance KnownNat n => Arbitrary (Finite n) where
  arbitrary = elements finites

instance (Arbitrary a, KnownNat n, G.Vector v a) => Arbitrary (SG.Vector v n a) where
  arbitrary = SG.replicateM arbitrary
  shrink vs = [ vs SG.// [(i, x)] | i <- finites, x <- shrink (SG.index vs i) ]

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

instance (Eq a, Semiring a, Arbitrary a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Arbitrary (MultiPoly v n a) where
  arbitrary = toMultiPoly . G.fromList <$> arbitrary
  shrink = fmap (toMultiPoly . G.fromList) . shrink . G.toList . unMultiPoly

instance (Eq a, Semiring a, Arbitrary a, KnownNat n, G.Vector v (SU.Vector n Word, a)) => Arbitrary (ShortPoly (MultiPoly v n a)) where
  arbitrary = ShortPoly . toMultiPoly . G.fromList . (\xs -> take (length xs `mod` 4) (map (first (SU.map (`mod` 3))) xs)) <$> arbitrary
  shrink = fmap (ShortPoly . toMultiPoly . G.fromList) . shrink . G.toList . unMultiPoly . unShortPoly

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (Word, a), G.Vector v (SU.Vector 1 Word, a)) => Arbitrary (SparseLaurent.Laurent v a) where
  arbitrary = SparseLaurent.toLaurent <$> ((`rem` 10) <$> arbitrary) <*> arbitrary
  shrink = fmap (uncurry SparseLaurent.toLaurent) . shrink . SparseLaurent.unLaurent

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (Word, a), G.Vector v (SU.Vector 1 Word, a)) => Arbitrary (ShortPoly (SparseLaurent.Laurent v a)) where
  arbitrary = (ShortPoly .) . SparseLaurent.toLaurent <$> ((`rem` 10) <$> arbitrary) <*> (unShortPoly <$> arbitrary)
  shrink = fmap (ShortPoly . uncurry SparseLaurent.toLaurent . fmap unShortPoly) . shrink . fmap ShortPoly . SparseLaurent.unLaurent . unShortPoly

-------------------------------------------------------------------------------

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
