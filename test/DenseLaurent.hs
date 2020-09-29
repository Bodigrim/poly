{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module DenseLaurent
  ( testSuite
  ) where

import Prelude hiding (gcd, quotRem, quot, rem)
import Control.Exception
import Data.Euclidean (GcdDomain(..), Field)
import Data.Int
import qualified Data.Poly
import Data.Poly.Laurent
import Data.Proxy
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

import Quaternion
import TestUtils

testSuite :: TestTree
testSuite = testGroup "DenseLaurent"
  [ otherTests
  , divideByZeroTests
  , lawsTests
  , evalTests
  , derivTests
  , patternTests
  ]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  $ semiringTests ++ ringTests ++ numTests ++ gcdDomainTests ++ showTests

semiringTests :: [TestTree]
semiringTests =
  [ mySemiringLaws (Proxy :: Proxy (ULaurent ()))
  , mySemiringLaws (Proxy :: Proxy (ULaurent Int8))
  , mySemiringLaws (Proxy :: Proxy (VLaurent Integer))
  , mySemiringLaws (Proxy :: Proxy (ULaurent (Quaternion Int)))
  ]

ringTests :: [TestTree]
ringTests =
  [ myRingLaws (Proxy :: Proxy (ULaurent ()))
  , myRingLaws (Proxy :: Proxy (ULaurent Int8))
  , myRingLaws (Proxy :: Proxy (VLaurent Integer))
  , myRingLaws (Proxy :: Proxy (ULaurent (Quaternion Int)))
  ]

numTests :: [TestTree]
numTests =
  [ myNumLaws (Proxy :: Proxy (ULaurent Int8))
  , myNumLaws (Proxy :: Proxy (VLaurent Integer))
  , myNumLaws (Proxy :: Proxy (ULaurent (Quaternion Int)))
  ]

gcdDomainTests :: [TestTree]
gcdDomainTests =
  [ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VLaurent Integer)))
  , myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VLaurent Rational)))
  ]

showTests :: [TestTree]
showTests =
  [ myShowLaws (Proxy :: Proxy (ULaurent ()))
  , myShowLaws (Proxy :: Proxy (ULaurent Int8))
  , myShowLaws (Proxy :: Proxy (VLaurent Integer))
  , myShowLaws (Proxy :: Proxy (ULaurent (Quaternion Int)))
  ]

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
  [ testProperty "leading p 0 == Nothing" $
    \p -> leading (monomial p 0 :: ULaurent a) === Nothing
  , testProperty "leading . monomial = id" $
    \p c -> c /= 0 ==> leading (monomial p c :: ULaurent a) === Just (p, c)
  , tenTimesLess $
    testProperty "scale matches multiplication by monomial" $
    \p c (xs :: ULaurent a) -> scale p c xs === monomial p c * xs
  , tenTimesLess $
    testProperty "toLaurent . unLaurent" $
    \(xs :: ULaurent a) -> uncurry toLaurent (unLaurent xs) === xs
  ]

divideByZeroTests :: TestTree
divideByZeroTests = testGroup "divideByZero"
  [ testProperty "divide"  $ testProp divide
  ]
  where
    testProp f xs = ioProperty ((== Left DivideByZero) <$> try (evaluate (xs `f` (0 :: VLaurent Rational))))

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup  (Proxy :: Proxy (VLaurent Rational))
  , substTestGroup (Proxy :: Proxy (ULaurent Int8))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Field a, Arbitrary a, Show a, Eq (v a), Show (v a), G.Vector v a)
  => Proxy (Laurent v a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) r = eval p r + eval q r" $
    \(ShortPoly p) (ShortPoly q) r -> e (p `plus` q) r === e p r `plus` e q r
  , testProperty "eval (p * q) r = eval p r * eval q r" $
    \(ShortPoly p) (ShortPoly q) r -> e (p `times` q) r === e p r `times` e q r
  , testProperty "eval x p = p" $
    \p -> e X p === p
  , testProperty "eval (monomial 0 c) p = c" $
    \c p -> e (monomial 0 c) p === c
  ]
  where
    e :: Laurent v a -> a -> a
    e = eval

substTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v a), Show (v a), G.Vector v a)
  => Proxy (Laurent v a)
  -> [TestTree]
substTestGroup _ =
  [ tenTimesLess $ tenTimesLess $ tenTimesLess $
    testProperty "subst (p + q) r = subst p r + subst q r" $
    \p q (ShortPoly r) -> e (p + q) r === e p r + e q r
  , testProperty "subst x p = p" $
    \p -> e Data.Poly.X p === p
  , testProperty "subst (monomial 0 c) p = monomial 0 c" $
    \c p -> e (Data.Poly.monomial 0 c) p === monomial 0 c
  ]
  where
    e :: Data.Poly.Poly v a -> Laurent v a -> Laurent v a
    e = subst

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv c = 0" $
    \c -> deriv (monomial 0 c :: ULaurent Int) === 0
  , testProperty "deriv cX = c" $
    \c -> deriv (monomial 0 c * X :: ULaurent Int) === monomial 0 c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \p q -> deriv (p + q) === (deriv p + deriv q :: ULaurent Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \p q -> deriv (p * q) === (p * deriv q + q * deriv p :: ULaurent Int)
  , tenTimesLess $ tenTimesLess $ tenTimesLess $
    testProperty "deriv (subst p q) = deriv q * subst (deriv p) q" $
    \(ShortPoly (p :: Data.Poly.UPoly Int)) (ShortPoly (q :: ULaurent Int)) ->
      deriv (subst p q) === deriv q * subst (Data.Poly.deriv p) q
  ]

patternTests :: TestTree
patternTests = testGroup "pattern"
  [ testProperty "X  :: ULaurent Int" $ once $
    case (monomial 1 1 :: ULaurent Int) of X -> True; _ -> False
  , testProperty "X  :: ULaurent Int" $ once $
    (X :: ULaurent Int) === monomial 1 1
  , testProperty "X :: ULaurent ()" $ once $
    case (zero :: ULaurent ()) of X -> True; _ -> False
  , testProperty "X :: ULaurent ()" $ once $
    (X :: ULaurent ()) === zero
  , testProperty "X^-k" $
    \(NonNegative j) k -> ((X^j)^-k :: ULaurent Int) === monomial (- j * k) 1
  , testProperty "^-" $
    \(p :: ULaurent Int) (NonNegative k) -> ioProperty $ do
      et <- try (evaluate (p^-k)) :: IO (Either PatternMatchFail (ULaurent Int))
      pure $ case et of
        Left{}  -> True
        Right t -> p^k * t == one
  ]
