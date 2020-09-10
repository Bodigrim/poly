{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module DFT
  ( testSuite
  ) where

import Data.Complex
import Data.Mod
import Data.Poly.Semiring (UPoly, unPoly, toPoly, dft, inverseDft, dftMult)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.Tasty
import Test.Tasty.QuickCheck hiding (scale, numTests)

#if MIN_VERSION_base(4,10,0)
import GHC.TypeNats (KnownNat, natVal, type (+), type (^))
#else
import GHC.TypeLits (KnownNat, natVal, type (+), type (^))
#endif

import Dense ()

testSuite :: TestTree
testSuite = testGroup "DFT"
  [ testGroup "dft matches reference"
    [ dftMatchesRef (0 :: Mod (2 ^ 0 + 1))
    , dftMatchesRef (2 :: Mod (2 ^ 1 + 1))
    , dftMatchesRef (2 :: Mod (2 ^ 2 + 1))
    , dftMatchesRef (3 :: Mod (2 ^ 4 + 1))
    , dftMatchesRef (3 :: Mod (2 ^ 8 + 1))
    ]
  , testGroup "dft is invertible"
    [ dftIsInvertible (0 :: Mod (2 ^ 0 + 1))
    , dftIsInvertible (2 :: Mod (2 ^ 1 + 1))
    , dftIsInvertible (2 :: Mod (2 ^ 2 + 1))
    , dftIsInvertible (3 :: Mod (2 ^ 4 + 1))
    , dftIsInvertible (3 :: Mod (2 ^ 8 + 1))
    ]
  , testProperty "dftMult matches reference" dftMultMatchesRef
  ]

dftMatchesRef :: KnownNat n1 => Mod n1 -> TestTree
dftMatchesRef primRoot = testProperty (show n) $ do
  xs <- V.replicateM n arbitrary
  pure $ dft primRoot xs === dftRef primRoot xs
  where
    n = fromIntegral (natVal primRoot - 1)

dftRef :: Num a => a -> V.Vector a -> V.Vector a
dftRef primRoot xs = V.generate (V.length xs) $
  \k -> sum (map (\j -> xs V.! j * primRoot ^ (j * k)) [0..n-1])
  where
    n = length xs

dftIsInvertible :: KnownNat n1 => Mod n1 -> TestTree
dftIsInvertible primRoot = testProperty (show n) $ do
  xs <- V.replicateM n arbitrary
  let ys = dft primRoot xs
      zs = inverseDft primRoot ys
  pure $ xs === zs
  where
    n = fromIntegral (natVal primRoot - 1)

dftMultMatchesRef :: UPoly Int -> UPoly Int -> Property
dftMultMatchesRef xs ys = zs === dftZs
  where
    xs', ys', dftZs' :: UPoly (Complex Double)
    xs' = toPoly $ U.map fromIntegral $ unPoly xs
    ys' = toPoly $ U.map fromIntegral $ unPoly ys
    dftZs' = dftMult (\k -> cis (2 * pi / fromIntegral k)) xs' ys'

    zs, dftZs :: UPoly (Complex Int)
    zs  = toPoly $ U.map (:+ 0) $ unPoly $ xs * ys
    dftZs  = toPoly $ U.map (\(x :+ y) -> round x :+ round y) $ unPoly dftZs'
