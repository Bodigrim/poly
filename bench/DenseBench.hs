{-# LANGUAGE RankNTypes #-}

module DenseBench
  ( benchSuite
  ) where

import Gauge.Main
import Data.Poly
import qualified Data.Vector.Unboxed as U

benchSuite :: Benchmark
benchSuite = bgroup "dense" $ concat
  [ map benchAdd      [100, 1000, 10000]
  , map benchMul      [10, 100]
  , map benchEval     [100, 1000, 10000]
  , map benchDeriv    [100, 1000, 10000]
  , map benchIntegral [100, 1000, 10000]
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
