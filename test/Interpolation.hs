{-# LANGUAGE ScopedTypeVariables #-}

module Interpolation (testSuite) where

import Data.Function (on)
import Data.List (nubBy)
import Data.Poly hiding (scale)
import Data.Poly.Interpolation
import Test.Tasty
import Test.Tasty.QuickCheck

import TestUtils ()

testSuite :: TestTree
testSuite = localOption (QuickCheckMaxSize 10) $ testGroup "Interpolation"
  [ testProperty "lagrange interpolates" $ \xys -> prop_lagrange (nubBy ((==) `on` fst) xys)
  , testProperty "hermite interpolates"  $ \xys -> prop_hermite (nubBy ((==) `on` (\(x, _, _, _, _) -> x)) xys)
  , testProperty "lagrange == hermite"   $ \xys -> prop_lagrange_hermite (nubBy ((==) `on` fst) xys)
  ]

prop_lagrange :: [(Rational, Rational)] -> Property
prop_lagrange xys =
  let p = lagrange xys :: VPoly Rational
  in conjoin $ map (\(x, y) -> eval p x === y) xys

prop_hermite :: [(Rational, Rational, Rational, Rational, Rational)] -> Property
prop_hermite xys =
  let
    p = hermite (map (\(x, y, y', y'', y''') -> (x, [y, y', y'', y'''])) xys) :: VPoly Rational
    p' = deriv p
    p'' = deriv p'
    p''' = deriv p''
  in conjoin $ map (\(x, y, y', y'', y''') -> eval p x === y .&&. eval p' x === y' .&&. eval p'' x === y'' .&&. eval p''' x === y''') xys

prop_lagrange_hermite :: [(Rational, Rational)] -> Property
prop_lagrange_hermite xys =
  let
    p = lagrange xys :: VPoly Rational
    q = hermite (map (\(x, y) -> (x, [y])) xys) :: VPoly Rational
  in p === q
