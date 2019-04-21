{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import Data.Int
import Data.Poly
import Data.Proxy
import Data.Semiring
import qualified Data.Vector as V
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes

main :: IO ()
main = defaultMain $ testGroup "All"
    [ semiringTests
    ]

semiringTests :: TestTree
semiringTests
  = testGroup "Semiring"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ semiringLaws (Proxy :: Proxy (Poly ()))
  ,     ringLaws (Proxy :: Proxy (Poly ()))
  , semiringLaws (Proxy :: Proxy (Poly Int8))
  ,     ringLaws (Proxy :: Proxy (Poly Int8))
  , semiringLaws (Proxy :: Proxy (Poly Integer))
  ,     ringLaws (Proxy :: Proxy (Poly Integer))
  ]

instance (Eq a, Semiring a, Arbitrary a) => Arbitrary (Poly a) where
  arbitrary = toPoly' . V.fromList <$> arbitrary
  shrink = fmap (toPoly' . V.fromList) . shrink . V.toList . unPoly
