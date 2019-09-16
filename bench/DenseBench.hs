{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module DenseBench
  ( benchSuite
  ) where

import Prelude hiding (quotRem, gcd)
import Gauge.Main
import Data.Poly
import qualified Data.Vector.Unboxed as U
#if MIN_VERSION_semirings(0,4,2)
import Data.Euclidean
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
  , map benchQuotRem  [10, 100]
  , map benchGcd      [10, 100]
  , map benchGcdExt   [10, 100]
  , map benchGcdFrac  [10, 100]
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

benchGcdExt :: Int -> Benchmark
benchGcdExt k = bench ("gcdExt/" ++ show k) $ nf doGcdExt k

benchGcdFrac :: Int -> Benchmark
benchGcdFrac k = bench ("gcdFrac/" ++ show k) $ nf doGcdFrac k

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

doQuotRem :: Int -> Double
doQuotRem n = U.sum (unPoly qs) + U.sum (unPoly rs)
  where
    xs = toPoly $ U.generate (2 * n) ((+ 1.0) . (* 2.0) . fromIntegral)
    ys = toPoly $ U.generate n       ((+ 2.0) . (* 3.0) . fromIntegral)
    (qs, rs) = xs `quotRem` ys

doGcd :: Int -> Integer
doGcd n = V.sum gs
  where
    xs = toPoly $ V.generate n ((+ 1) . (* 2) . fromIntegral)
    ys = toPoly $ V.generate n ((+ 2) . (* 3) . fromIntegral)
    gs = unPoly $ xs `gcd` ys

doGcdExt :: Int -> Rational
doGcdExt n = V.sum gs
  where
    xs = toPoly $ V.generate n ((+ 1) . (* 2) . fromIntegral)
    ys = toPoly $ V.generate n ((+ 2) . (* 3) . fromIntegral)
    gs = unPoly $ fst $ xs `gcdExt` ys

doGcdFrac :: Int -> Rational
doGcdFrac n = V.sum gs
  where
    xs = PolyOverField $ toPoly $ V.generate n ((+ 1) . (* 2) . fromIntegral)
    ys = PolyOverField $ toPoly $ V.generate n ((+ 2) . (* 3) . fromIntegral)
    gs = unPoly $ unPolyOverField $ xs `gcd` ys

#endif
