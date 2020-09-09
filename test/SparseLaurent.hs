{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module SparseLaurent
  ( testSuite
  ) where

import Prelude hiding (gcd, quotRem, rem)
import Data.Euclidean (Euclidean(..), GcdDomain(..), Field)
import Data.Int
import qualified Data.Poly.Sparse
import Data.Poly.Sparse.Laurent
import Data.Proxy
import Data.Semiring (Semiring(..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

import Quaternion
import Sparse (ShortPoly(..))
import TestUtils

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (Word, a)) => Arbitrary (Laurent v a) where
  arbitrary = toLaurent <$> ((`rem` 10) <$> arbitrary) <*> arbitrary
  shrink = fmap (uncurry toLaurent) . shrink . unLaurent

newtype ShortLaurent a = ShortLaurent { unShortLaurent :: a }
  deriving (Eq, Show, Semiring, GcdDomain)

instance (Eq a, Semiring a, Arbitrary a, G.Vector v (Word, a)) => Arbitrary (ShortLaurent (Laurent v a)) where
  arbitrary = (ShortLaurent .) . toLaurent <$> ((`rem` 10) <$> arbitrary) <*> (unShortPoly <$> arbitrary)
  shrink = fmap (ShortLaurent . uncurry toLaurent . fmap unShortPoly) . shrink . fmap ShortPoly . unLaurent . unShortLaurent

testSuite :: TestTree
testSuite = testGroup "SparseLaurent"
  [ otherTests
  , lawsTests
  , evalTests
  , derivTests
  , patternTests
  ]

lawsTests :: TestTree
lawsTests = testGroup "Laws"
  $ semiringTests ++ ringTests ++ numTests ++ gcdDomainTests ++ isListTests ++ showTests

semiringTests :: [TestTree]
semiringTests =
  [ mySemiringLaws (Proxy :: Proxy (Laurent U.Vector ()))
  , mySemiringLaws (Proxy :: Proxy (Laurent U.Vector Int8))
  , mySemiringLaws (Proxy :: Proxy (Laurent V.Vector Integer))
  , tenTimesLess
  $ mySemiringLaws (Proxy :: Proxy (Laurent U.Vector (Quaternion Int)))
  ]

ringTests :: [TestTree]
ringTests =
  [ myRingLaws (Proxy :: Proxy (Laurent U.Vector ()))
  , myRingLaws (Proxy :: Proxy (Laurent U.Vector Int8))
  , myRingLaws (Proxy :: Proxy (Laurent V.Vector Integer))
  , myRingLaws (Proxy :: Proxy (Laurent U.Vector (Quaternion Int)))
  ]

numTests :: [TestTree]
numTests =
  [ myNumLaws (Proxy :: Proxy (Laurent U.Vector Int8))
  , myNumLaws (Proxy :: Proxy (Laurent V.Vector Integer))
  , tenTimesLess
  $ myNumLaws (Proxy :: Proxy (Laurent U.Vector (Quaternion Int)))
  ]

gcdDomainTests :: [TestTree]
gcdDomainTests =
  [ myGcdDomainLaws (Proxy :: Proxy (ShortLaurent (Laurent V.Vector Integer)))
  , tenTimesLess
  $ myGcdDomainLaws (Proxy :: Proxy (ShortLaurent (Laurent V.Vector Rational)))
  ]

isListTests :: [TestTree]
isListTests =
  [ myIsListLaws (Proxy :: Proxy (Laurent U.Vector ()))
  , myIsListLaws (Proxy :: Proxy (Laurent U.Vector Int8))
  , myIsListLaws (Proxy :: Proxy (Laurent V.Vector Integer))
  , tenTimesLess
  $ myIsListLaws (Proxy :: Proxy (Laurent U.Vector (Quaternion Int)))
  ]

showTests :: [TestTree]
showTests =
  [ myShowLaws (Proxy :: Proxy (Laurent U.Vector ()))
  , myShowLaws (Proxy :: Proxy (Laurent U.Vector Int8))
  , myShowLaws (Proxy :: Proxy (Laurent V.Vector Integer))
  , tenTimesLess
  $ myShowLaws (Proxy :: Proxy (Laurent U.Vector (Quaternion Int)))
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

evalTests :: TestTree
evalTests = testGroup "eval" $ concat
  [ evalTestGroup  (Proxy :: Proxy (Laurent V.Vector Rational))
  , substTestGroup (Proxy :: Proxy (Laurent U.Vector Int8))
  ]

evalTestGroup
  :: forall v a.
     (Eq a, Field a, Arbitrary a, Show a, Eq (v (Word, a)), Show (v (Word, a)), G.Vector v (Word, a))
  => Proxy (Laurent v a)
  -> [TestTree]
evalTestGroup _ =
  [ testProperty "eval (p + q) r = eval p r + eval q r" $
    \p q r -> e (p `plus` q) r === e p r `plus` e q r
  , testProperty "eval (p * q) r = eval p r * eval q r" $
    \p q r -> e (p `times` q) r === e p r `times` e q r
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
     (Eq a, Num a, Semiring a, Arbitrary a, Show a, Eq (v (Word, a)), Show (v (Word, a)), G.Vector v (Word, a))
  => Proxy (Laurent v a)
  -> [TestTree]
substTestGroup _ =
  [ testProperty "subst x p = p" $
    \p -> e Data.Poly.Sparse.X p === p
  , testProperty "subst (monomial 0 c) p = monomial 0 c" $
    \c p -> e (Data.Poly.Sparse.monomial 0 c) p === monomial 0 c
  ]
  where
    e :: Data.Poly.Sparse.Poly v a -> Laurent v a -> Laurent v a
    e = subst

derivTests :: TestTree
derivTests = testGroup "deriv"
  [ testProperty "deriv c = 0" $
    \c -> deriv (monomial 0 c :: Laurent V.Vector Int) === 0
  , testProperty "deriv cX = c" $
    \c -> deriv (monomial 0 c * X :: Laurent V.Vector Int) === monomial 0 c
  , testProperty "deriv (p + q) = deriv p + deriv q" $
    \p q -> deriv (p + q) === (deriv p + deriv q :: Laurent V.Vector Int)
  , testProperty "deriv (p * q) = p * deriv q + q * deriv p" $
    \p q -> deriv (p * q) === (p * deriv q + q * deriv p :: Laurent V.Vector Int)
  -- , testProperty "deriv (subst p q) = deriv q * subst (deriv p) q" $
  --   \(p :: Laurent V.Vector Int) (q :: Laurent U.Vector Int) ->
  --     deriv (subst p q) === deriv q * subst (deriv p) q
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
    \k -> (X^-k :: ULaurent Int) === monomial (- k) 1
  ]
