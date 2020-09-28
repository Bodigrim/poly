{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

module MultiLaurent
  ( testSuite
  ) where

import Prelude hiding (gcd, quotRem, quot, rem)
import Control.Exception
import Data.Euclidean (GcdDomain(..), Field)
import Data.Int
import qualified Data.Poly.Multi
import Data.Poly.Multi.Laurent
import Data.Proxy
import Data.Semiring (Semiring(..))
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Sized as SV
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Sized as SU
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

import Quaternion
import TestUtils

testSuite :: TestTree
testSuite = testGroup "MultiLaurent"
  [ otherTests
  , divideByZeroTests
  , lawsTests
  , evalTests
  , derivTests
  , patternTests
  , conversionTests
  ]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  $ semiringTests ++ ringTests ++ numTests ++ gcdDomainTests ++ isListTests ++ showTests

semiringTests :: [TestTree]
semiringTests =
  [ mySemiringLaws (Proxy :: Proxy (UMultiLaurent 3 ()))
  , mySemiringLaws (Proxy :: Proxy (UMultiLaurent 3 Int8))
  , mySemiringLaws (Proxy :: Proxy (VMultiLaurent 3 Integer))
  , tenTimesLess
  $ mySemiringLaws (Proxy :: Proxy (UMultiLaurent 3 (Quaternion Int)))
  ]

ringTests :: [TestTree]
ringTests =
  [ myRingLaws (Proxy :: Proxy (UMultiLaurent 3 ()))
  , myRingLaws (Proxy :: Proxy (UMultiLaurent 3 Int8))
  , myRingLaws (Proxy :: Proxy (VMultiLaurent 3 Integer))
  , myRingLaws (Proxy :: Proxy (UMultiLaurent 3 (Quaternion Int)))
  ]

numTests :: [TestTree]
numTests =
  [ myNumLaws (Proxy :: Proxy (UMultiLaurent 3 Int8))
  , myNumLaws (Proxy :: Proxy (VMultiLaurent 3 Integer))
  , tenTimesLess
  $ myNumLaws (Proxy :: Proxy (UMultiLaurent 3 (Quaternion Int)))
  ]

gcdDomainTests :: [TestTree]
gcdDomainTests =
  [ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VMultiLaurent 3 Integer)))
  , tenTimesLess
  $ myGcdDomainLaws (Proxy :: Proxy (ShortPoly (VMultiLaurent 3 Rational)))
  ]

isListTests :: [TestTree]
isListTests =
  [ myIsListLaws (Proxy :: Proxy (UMultiLaurent 3 ()))
  , myIsListLaws (Proxy :: Proxy (UMultiLaurent 3 Int8))
  , myIsListLaws (Proxy :: Proxy (VMultiLaurent 3 Integer))
  , tenTimesLess
  $ myIsListLaws (Proxy :: Proxy (UMultiLaurent 3 (Quaternion Int)))
  ]

showTests :: [TestTree]
showTests =
  [ myShowLaws (Proxy :: Proxy (UMultiLaurent 4 ()))
  , myShowLaws (Proxy :: Proxy (UMultiLaurent 4 Int8))
  , myShowLaws (Proxy :: Proxy (VMultiLaurent 4 Integer))
  , tenTimesLess
  $ myShowLaws (Proxy :: Proxy (UMultiLaurent 4 (Quaternion Int)))
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
  [ testProperty "scale matches multiplication by monomial" $
    \p c (xs :: UMultiLaurent 3 a) -> scale p c xs === monomial p c * xs
  , tenTimesLess $
    testProperty "toMultiLaurent . unMultiLaurent" $
    \(xs :: UMultiLaurent 3 a) -> uncurry toMultiLaurent (unMultiLaurent xs) === xs
  ]

divideByZeroTests :: TestTree
divideByZeroTests = testGroup "divideByZero"
  [ testProperty "divide"  $ testProp divide
  ]
  where
    testProp f xs = ioProperty ((== Left DivideByZero) <$> try (evaluate (xs `f` (0 :: VMultiLaurent 3 Rational))))

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup  (Proxy :: Proxy (VMultiLaurent 3 Rational))
  , substTestGroup (Proxy :: Proxy (UMultiLaurent 3 Int8))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Field a, Arbitrary a, Show a, Eq (v (Word, a)), Show (v (Word, a)), G.Vector v (Word, a), Eq (v (SU.Vector 3 Word, a)), Show (v (SU.Vector 3 Word, a)), G.Vector v (SU.Vector 3 Word, a))
  => Proxy (MultiLaurent v 3 a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) r = eval p r + eval q r" $
    \p q r -> e (p `plus` q) r === e p r `plus` e q r
  , testProperty "eval (p * q) r = eval p r * eval q r" $
    \p q r -> e (p `times` q) r === e p r `times` e q r
  , testProperty "eval x p = p" $
    \p -> e X (SV.fromTuple (p, undefined, undefined)) === p
  , testProperty "eval (monomial 0 c) p = c" $
    \c p -> e (monomial 0 c) p === c
  ]
  where
    e :: MultiLaurent v 3 a -> SV.Vector 3 a -> a
    e = eval

substTestGroup
  :: forall v a.
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v (SU.Vector 3 Word, a)), Show (v (Word, a)), G.Vector v (Word, a), G.Vector v (SU.Vector 3 Word, a))
  => Proxy (MultiLaurent v 3 a)
  -> [TestTree]
substTestGroup _ =
  [ testProperty "subst x p = p" $
    \p -> e Data.Poly.Multi.X (SV.fromTuple (p, undefined, undefined)) === p
  , testProperty "subst (monomial 0 c) p = monomial 0 c" $
    \c p -> e (Data.Poly.Multi.monomial 0 c) p === monomial 0 c
  ]
  where
    e :: Data.Poly.Multi.MultiPoly v 3 a -> SV.Vector 3 (MultiLaurent v 3 a) -> MultiLaurent v 3 a
    e = subst

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv c = 0" $
    \k c -> deriv k (monomial 0 c :: VMultiLaurent 3 Int) === 0
  , testProperty "deriv cX = c" $
    \k c -> deriv k (monomial 0 c * X :: VMultiLaurent 3 Int) === monomial 0 c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \k p q -> deriv k (p + q) === (deriv k p + deriv k q :: VMultiLaurent 3 Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \k p q -> deriv k (p * q) === (p * deriv k q + q * deriv k p :: VMultiLaurent 3 Int)
  ]

patternTests :: TestTree
patternTests = testGroup "pattern"
  [ testProperty "X  :: UMultiLaurent Int" $ once $
    case (monomial 1 1 :: UMultiLaurent 1 Int) of X -> True; _ -> False
  , testProperty "X  :: UMultiLaurent Int" $ once $
    (X :: UMultiLaurent 1 Int) === monomial 1 1
  , testProperty "X :: UMultiLaurent ()" $ once $
    case (zero :: UMultiLaurent 1 ()) of X -> True; _ -> False
  , testProperty "X :: UMultiLaurent ()" $ once $
    (X :: UMultiLaurent 1 ()) === zero

  , testProperty "Y  :: UMultiLaurent Int" $ once $
    case (monomial (SG.fromTuple (0, 1)) 1 :: UMultiLaurent 2 Int) of Y -> True; _ -> False
  , testProperty "Y  :: UMultiLaurent Int" $ once $
    (Y :: UMultiLaurent 2 Int) === monomial (SG.fromTuple (0, 1)) 1
  , testProperty "Y :: UMultiLaurent ()" $ once $
    case (zero :: UMultiLaurent 2 ()) of Y -> True; _ -> False
  , testProperty "Y :: UMultiLaurent ()" $ once $
    (Y :: UMultiLaurent 2 ()) === zero

  , testProperty "Z  :: UMultiLaurent Int" $ once $
    case (monomial (SG.fromTuple (0, 0, 1)) 3 :: UMultiLaurent 3 Int) of Z -> True; _ -> False
  , testProperty "Z  :: UMultiLaurent Int" $ once $
    (Z :: UMultiLaurent 3 Int) === monomial (SG.fromTuple (0, 0, 1)) 1
  , testProperty "Z :: UMultiLaurent ()" $ once $
    case (zero :: UMultiLaurent 3 ()) of Z -> True; _ -> False
  , testProperty "Z :: UMultiLaurent ()" $ once $
    (Z :: UMultiLaurent 3 ()) === zero

  , testProperty "X^-k" $
    \(NonNegative j) k -> ((X^j)^-k :: UMultiLaurent 1 Int) === monomial (SG.singleton (- j * k)) 1
  , testProperty "Y^-k" $
    \(NonNegative j) k -> ((Y^j)^-k :: UMultiLaurent 2 Int) === monomial (SG.fromTuple (0, - j * k)) 1
  , testProperty "Z^-k" $
    \(NonNegative j) k -> ((Z^j)^-k :: UMultiLaurent 3 Int) === monomial (SG.fromTuple (0, 0, - j * k)) 1
  , testProperty "^-" $
    \(p :: UMultiLaurent 3 Int) (NonNegative k) -> ioProperty $ do
      et <- try (evaluate (p^-k)) :: IO (Either PatternMatchFail (UMultiLaurent 3 Int))
      pure $ case et of
        Left{}  -> True
        Right t -> p^k * t == one
  ]

conversionTests :: TestTree
conversionTests = testGroup "conversions"
  [ testProperty "unsegregate . segregate = id" $
    \(xs :: UMultiLaurent 3 Int8) -> xs === unsegregate (segregate xs)
  , testProperty "segregate . unsegregate = id" $
    \xs -> xs === segregate (unsegregate xs :: UMultiLaurent 3 Int8)
  ]
