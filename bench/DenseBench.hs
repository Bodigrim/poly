{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeApplications           #-}

module DenseBench
  ( benchSuite
  ) where

import Prelude hiding (quotRem, gcd)
import Control.DeepSeq
import Gauge.Main
import Data.Poly
import qualified Data.Vector.Unboxed as U
#if MIN_VERSION_semirings(0,4,2)
import Control.Exception
import Data.Bits
import Data.Coerce
import Data.Euclidean
import Data.Semiring (Semiring(..), Ring, isZero)
import qualified Data.Semiring as S (negate)
import qualified Data.Vector as V
#endif

benchSuite :: Benchmark
benchSuite = bgroup "dense" $ concat
  [ map benchAdd      [100, 1000, 10000]
  , map benchMul      [100, 1000, 10000]
  , map benchEval     [100, 1000, 10000]
  , map benchDeriv    [100, 1000, 10000]
  , map benchIntegral [100, 1000, 10000]
#if MIN_VERSION_semirings(0,4,2)
  , map benchQuotRem    [10, 100]
  , map benchGcd        [10, 100]
  , map benchGcdExtRat  [10, 20, 40]
  , map benchGcdFracRat [10, 20, 40]
  , map benchGcdExtM    [10, 100, 1000]
  , map benchGcdFracM   [10, 100, 1000]
#endif
  ]

benchAdd :: Int -> Benchmark
benchAdd k = bench ("add/" ++ show k) $ nf (doBinOp (+)) k

benchMul :: Int -> Benchmark
benchMul k = bench ("mul/" ++ show k) $ nf (doBinOp (*)) k

benchEval :: Int -> Benchmark
benchEval k = bench ("eval/" ++ show k) $ nf doEval k

benchDeriv :: Int -> Benchmark
benchDeriv k = bench ("deriv/" ++ show k) $ nf doDeriv k

benchIntegral :: Int -> Benchmark
benchIntegral k = bench ("integral/" ++ show k) $ nf doIntegral k

#if MIN_VERSION_semirings(0,4,2)

benchQuotRem :: Int -> Benchmark
benchQuotRem k = bench ("quotRem/" ++ show k) $ nf doQuotRem k

benchGcd :: Int -> Benchmark
benchGcd k = bench ("gcd/" ++ show k) $ nf doGcd k

benchGcdExtRat :: Int -> Benchmark
benchGcdExtRat k = bench ("gcdExt/Rational/" ++ show k) $ nf (doGcdExt @Rational) k

benchGcdFracRat :: Int -> Benchmark
benchGcdFracRat k = bench ("gcdFrac/Rational/" ++ show k) $ nf (doGcdFrac @Rational) k

benchGcdExtM :: Int -> Benchmark
benchGcdExtM k = bench ("gcdExt/Mod2/" ++ show k) $ nf (doGcdExt @Mod2) k

benchGcdFracM :: Int -> Benchmark
benchGcdFracM k = bench ("gcdFrac/Mod2/" ++ show k) $ nf (doGcdFrac @Mod2) k

#endif

doBinOp :: (forall a. Num a => a -> a -> a) -> Int -> Int
doBinOp op n = U.sum zs
  where
    xs = toPoly $ U.generate n (* 2)
    ys = toPoly $ U.generate n (* 3)
    zs = unPoly $ xs `op` ys
{-# INLINE doBinOp #-}

doEval :: Int -> Int
doEval n = eval xs n
  where
    xs = toPoly $ U.generate n (* 2)

doDeriv :: Int -> Int
doDeriv n = U.sum zs
  where
    xs = toPoly $ U.generate n (* 2)
    zs = unPoly $ deriv xs

doIntegral :: Int -> Double
doIntegral n = U.sum zs
  where
    xs = toPoly $ U.generate n ((* 2) . fromIntegral)
    zs = unPoly $ integral xs

#if MIN_VERSION_semirings(0,4,2)

gen1 :: Num a => Int -> a
gen1 k = fromIntegral (truncate (pi * fromIntegral k :: Double) `mod` (k + 1))

gen2 :: Num a => Int -> a
gen2 k = fromIntegral (truncate (exp 1.0 * fromIntegral k :: Double) `mod` (k + 1))

doQuotRem :: Int -> Double
doQuotRem n = U.sum (unPoly qs) + U.sum (unPoly rs)
  where
    xs = toPoly $ U.generate (2 * n) gen1
    ys = toPoly $ U.generate n       gen2
    (qs, rs) = xs `quotRem` ys

doGcd :: Int -> Integer
doGcd n = V.sum gs
  where
    xs = toPoly $ V.generate n gen1
    ys = toPoly $ V.generate n gen2
    gs = unPoly $ xs `gcd` ys

doGcdExt :: (Eq a, Num a, Field a) => Int -> a
doGcdExt n = V.sum gs
  where
    xs = toPoly $ V.generate n gen1
    ys = toPoly $ V.generate n gen2
    gs = unPoly $ fst $ xs `gcdExt` ys

doGcdFrac :: (Eq a, Num a, Field a) => Int -> a
doGcdFrac n = V.sum gs
  where
    xs = PolyOverField $ toPoly $ V.generate n gen1
    ys = PolyOverField $ toPoly $ V.generate n gen2
    gs = unPoly $ unPolyOverField $ xs `gcd` ys

-- | Inspired by 'semirings'.
newtype Mod2 = Mod2 { _getMod2 :: Bool }
  deriving (Eq, NFData)

instance Num Mod2 where
  (+) = coerce (xor @Bool)
  (*) = coerce (&&)
  negate = id
  abs    = id
  signum = id
  fromInteger = Mod2 . odd

instance Semiring Mod2 where
  plus  = coerce (xor @Bool)
  times = coerce (&&)
  fromNatural = Mod2 . odd

instance Ring Mod2 where
  negate = id

instance GcdDomain Mod2 where

instance Euclidean Mod2 where
  degree = const 0
  quotRem x y
    | isZero y  = throw DivideByZero
    | otherwise = (x, zero)

instance Field Mod2

#endif
