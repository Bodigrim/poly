{-# LANGUAGE OverloadedLists #-}

module Orthogonal
  ( testSuite
  ) where

import Test.Tasty

import Data.List (foldl', tails)
import Data.Poly (VPoly, deriv, eval, integral)
import Data.Poly.Orthogonal
import Test.Tasty.QuickCheck

testSuite :: TestTree
testSuite = testGroup "Orthogonal"
  [ testGroup "differential equations"
    [ testProperty "jacobi"      prop_jacobi_de
    , testProperty "gegenbauer"  prop_gegenbauer_de
    , testProperty "legendre"    prop_legendre_de
    , testProperty "chebyshev1"  prop_chebyshev1_de
    , testProperty "chebyshev2"  prop_chebyshev2_de
    , testProperty "hermitePhys" prop_hermitePhys_de
    , testProperty "laguerre"    prop_laguerre_de
    , testProperty "laguerreGen" prop_laguerreGen_de
    ]
  , testGroup "normalization"
    [ testProperty "jacobi"     prop_jacobi_norm
    , testProperty "gegenbauer" prop_gegenbauer_norm
    , testProperty "legendre"   prop_legendre_norm
    , testProperty "chebyshev1" prop_chebyshev1_norm
    , testProperty "chebyshev2" prop_chebyshev2_norm
    ]
  , testGroup "orthogonality"
    [ testProperty "legendre"   prop_legendre_orth
    ]
  , testGroup "Hermite"
    [ testProperty "hermiteProb" prop_hermiteProb
    , testProperty "hermitePhys" prop_hermitePhys
    ]
  ]

prop_jacobi_de :: Rational -> Rational -> Property
prop_jacobi_de a b = foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] (jacobi a b)
  where
    de :: Rational -> VPoly Rational -> VPoly Rational
    de n y = [1, 0, -1] * deriv (deriv y)
           + [b - a, - (a + b + 2)] * deriv y
           + [n * (n + a + b + 1)] * y

prop_gegenbauer_de :: Rational -> Property
prop_gegenbauer_de g = foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] (gegenbauer g)
  where
    de :: Rational -> VPoly Rational -> VPoly Rational
    de n y = [1, 0, -1] * deriv (deriv y)
           + [0, - (2 * g + 1)] * deriv y
           + [n * (n + 2 * g)] * y

prop_legendre_de :: Property
prop_legendre_de = once $ foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] legendre
  where
    de :: Rational -> VPoly Rational -> VPoly Rational
    de n y = deriv ([1, 0, -1] * deriv y) + [n * (n + 1)] * y

prop_chebyshev1_de :: Property
prop_chebyshev1_de = once $ foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] chebyshev1
  where
    de :: Integer -> VPoly Integer -> VPoly Integer
    de n y = [1, 0, -1] * deriv (deriv y) + [0, -1] * deriv y + [n * n] * y

prop_chebyshev2_de :: Property
prop_chebyshev2_de = once $ foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] chebyshev2
  where
    de :: Integer -> VPoly Integer -> VPoly Integer
    de n y = [1, 0, -1] * deriv (deriv y) + [0, -3] * deriv y + [n * (n + 2)] * y

prop_hermitePhys_de :: Property
prop_hermitePhys_de = once $ foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] hermitePhys
  where
    de :: Integer -> VPoly Integer -> VPoly Integer
    de n y = deriv (deriv y) + [0, -2] * deriv y + [2 * n] * y

prop_laguerre_de :: Property
prop_laguerre_de = once $ foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] laguerre
  where
    de :: Rational -> VPoly Rational -> VPoly Rational
    de n y = [0, 1] * deriv (deriv y) + [1, -1] * deriv y + [n] * y

prop_laguerreGen_de :: Rational -> Property
prop_laguerreGen_de a  = foldl' (.&&.) (property True) $
  zipWith (((=== 0) .) . de) [0..limit] (laguerreGen a)
  where
    de :: Rational -> VPoly Rational -> VPoly Rational
    de n y = [0, 1] * deriv (deriv y) + [1 + a, -1] * deriv y + [n] * y

prop_jacobi_norm :: Rational -> Rational -> Property
prop_jacobi_norm a b = foldl' (.&&.) (property True) $
  zipWith (\n y -> norm n === eval y 1) [0..limit] (jacobi a b :: [VPoly Rational])
  where
    prod n x = product $ take n $ iterate (subtract 1) (fromIntegral n + x)
    norm n = prod n a / prod n 0

prop_gegenbauer_norm :: Rational -> Property
prop_gegenbauer_norm a = foldl' (.&&.) (property True) $
  zipWith (\n y -> norm n === eval y 1) [0..limit] (gegenbauer a :: [VPoly Rational])
  where
    prod n x = product $ take n $ iterate (subtract 1) (fromIntegral n + x)
    norm n = prod n (a - 1 / 2) / prod n 0

prop_legendre_norm :: Property
prop_legendre_norm = once $ foldl' (.&&.) (property True) $
  map ((=== 1) . flip eval 1) (take limit legendre :: [VPoly Rational])

prop_chebyshev1_norm :: Property
prop_chebyshev1_norm = once $ foldl' (.&&.) (property True) $
  map ((=== 1) . flip eval 1) (take limit chebyshev1 :: [VPoly Integer])

prop_chebyshev2_norm :: Property
prop_chebyshev2_norm = once $ foldl' (.&&.) (property True) $
  zipWith (\n y -> n + 1 === eval y 1) [0..limit] (chebyshev2 :: [VPoly Integer])

prop_legendre_orth :: Property
prop_legendre_orth = once $ foldl' (.&&.) (property True) $
  [ integral11 (x * y) === 0 | (x : xs) <- tails polys, y <- xs ]
  where
    polys :: [VPoly Rational]
    polys = take limit $ legendre

hermiteProbRef :: [VPoly Integer]
hermiteProbRef = iterate (\he -> [0, 1] * he - deriv he) 1

hermitePhysRef :: [VPoly Integer]
hermitePhysRef = iterate (\h -> [0, 2] * h - deriv h) 1

prop_hermiteProb :: Property
prop_hermiteProb = once $ foldl' (.&&.) (property True) $
  take limit $ zipWith (===) hermiteProb hermiteProbRef

prop_hermitePhys :: Property
prop_hermitePhys = once $ foldl' (.&&.) (property True) $
  take limit $ zipWith (===) hermitePhys hermitePhysRef

integral11 :: VPoly Rational -> Rational
integral11 x = eval y 1 - eval y (-1)
  where
    y = integral x

limit :: Num a => a
limit = 10
