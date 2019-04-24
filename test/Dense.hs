{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Dense
  ( testSuite
  ) where

import Prelude hiding (quotRem)
import Data.Int
import Data.Poly
import qualified Data.Poly.Semiring as S
import Data.Proxy
import Data.Semiring (Semiring)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.QuickCheck.Classes (lawsProperties, semiringLaws, ringLaws)

instance (Eq a, Semiring a, Arbitrary a, G.Vector v a) => Arbitrary (Poly v a) where
  arbitrary = S.toPoly . G.fromList <$> arbitrary
  shrink = fmap (S.toPoly . G.fromList) . shrink . G.toList . unPoly

testSuite :: TestTree
testSuite = testGroup "Dense"
    [ arithmeticTests
    , semiringTests
    , evalTests
    , derivTests
    , quotRemTests
    ]

semiringTests :: TestTree
semiringTests
  = testGroup "Semiring"
  $ map (uncurry testProperty)
  $ concatMap lawsProperties
  [ semiringLaws (Proxy :: Proxy (Poly U.Vector ()))
  ,     ringLaws (Proxy :: Proxy (Poly U.Vector ()))
  , semiringLaws (Proxy :: Proxy (Poly U.Vector Int8))
  ,     ringLaws (Proxy :: Proxy (Poly U.Vector Int8))
  , semiringLaws (Proxy :: Proxy (Poly V.Vector Integer))
  ,     ringLaws (Proxy :: Proxy (Poly V.Vector Integer))
  ]

arithmeticTests :: TestTree
arithmeticTests = testGroup "Arithmetic"
  [ testProperty "addition matches reference" $
    \(xs :: [Int]) ys -> toPoly (V.fromList (addRef xs ys)) ===
      toPoly (V.fromList xs) + toPoly (V.fromList ys)
  , testProperty "subtraction matches reference" $
    \(xs :: [Int]) ys -> toPoly (V.fromList (subRef xs ys)) ===
      toPoly (V.fromList xs) - toPoly (V.fromList ys)
  ]

addRef :: Num a => [a] -> [a] -> [a]
addRef [] ys = ys
addRef xs [] = xs
addRef (x : xs) (y : ys) = (x + y) : addRef xs ys

subRef :: Num a => [a] -> [a] -> [a]
subRef [] ys = map negate ys
subRef xs [] = xs
subRef (x : xs) (y : ys) = (x - y) : subRef xs ys

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup (Proxy :: Proxy (Poly U.Vector Int8))
  , evalTestGroup (Proxy :: Proxy (Poly V.Vector Integer))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v a), Show (v a), G.Vector v a)
  => Proxy (Poly v a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) r = eval p r + eval q r" $
    \p q r -> e (p + q) r === e p r + e q r
  , testProperty "eval (p * q) r = eval p r * eval q r" $
    \p q r -> e (p * q) r === e p r * e q r
  , testProperty "eval x p = p" $
    \p -> e X p === p
  , testProperty "eval (constant c) p = c" $
    \c p -> e (constant c) p === c

  , testProperty "eval' (p + q) r = eval' p r + eval' q r" $
    \p q r -> e' (p + q) r === e' p r + e' q r
  , testProperty "eval' (p * q) r = eval' p r * eval' q r" $
    \p q r -> e' (p * q) r === e' p r * e' q r
  , testProperty "eval' x p = p" $
    \p -> e' S.X p === p
  , testProperty "eval' (S.constant c) p = c" $
    \c p -> e' (S.constant c) p === c
  ]

  where
    e :: Poly v a -> a -> a
    e = eval
    e' :: Poly v a -> a -> a
    e' = S.eval

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv = S.deriv" $
    \(p :: Poly V.Vector Integer) -> deriv p === S.deriv p
  , testProperty "deriv . integral = id" $
    \(p :: Poly V.Vector Rational) -> deriv (integral p) === p
  , testProperty "deriv c = 0" $
    \c -> deriv (constant c :: Poly V.Vector Int) === 0
  , testProperty "deriv cX = c" $
    \c -> deriv (constant c * X :: Poly V.Vector Int) === constant c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \p q -> deriv (p + q) === (deriv p + deriv q :: Poly V.Vector Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \p q -> deriv (p * q) === (p * deriv q + q * deriv p :: Poly V.Vector Int)
  -- The following property takes too long for a regular test-suite
  -- , testProperty "deriv (eval p q) = deriv q * eval (deriv p) q" $
  --   \(p :: Poly V.Vector Int) (q :: Poly U.Vector Int) ->
  --     deriv (eval (toPoly $ fmap constant $ unPoly p) q) ===
  --       deriv q * eval (toPoly $ fmap constant $ unPoly $ deriv p) q
  ]

quotRemTests :: TestTree
quotRemTests = testGroup "quotRem" []
  -- [ testProperty "(q, r) = x `quotRem` y ==> q * y + r == x" $
  --   \(x :: Poly U.Vector Int) y -> let (q, r) = x `quotRem` y in
  --     y === 0 .||. q * y + r === x
  -- ]
