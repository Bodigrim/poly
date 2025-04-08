{-# LANGUAGE ScopedTypeVariables #-}

module Interpolation (testSuite) where

import Data.Map
import Data.Poly hiding (scale)
import Data.Poly.Interpolation
import Test.Tasty
import Test.Tasty.QuickCheck

import TestUtils ()

testSuite :: TestTree
testSuite = localOption (QuickCheckMaxSize 10) $ testGroup "Interpolation"
  [ testProperty "lagrange interpolates" prop_lagrange
  , testProperty "hermite interpolates"  prop_hermite
  , testProperty "lagrange == hermite"   prop_lagrange_hermite
  ]

prop_lagrange :: Map Rational Rational -> Property
prop_lagrange xys =
  let p = lagrange xys :: VPoly Rational
  in conjoin $ fmap (\(x, y) -> eval p x === y) (toList xys)

prop_hermite :: Map Rational (Rational, Rational, Rational, Rational) -> Property
prop_hermite xys =
  let
    p = hermite (fmap (\(y, y', y'', y''') -> [y, y', y'', y''']) xys) :: VPoly Rational
    p' = deriv p
    p'' = deriv p'
    p''' = deriv p''
  in conjoin $ fmap (\(x, (y, y', y'', y''')) -> eval p x === y .&&. eval p' x === y' .&&. eval p'' x === y'' .&&. eval p''' x === y''') (toList xys)

prop_lagrange_hermite :: Map Rational Rational -> Property
prop_lagrange_hermite xys =
  let
    p = lagrange xys :: VPoly Rational
    q = hermite (fmap (\y -> [y]) xys) :: VPoly Rational
  in p === q
