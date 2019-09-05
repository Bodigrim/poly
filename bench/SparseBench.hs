{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module SparseBench
  ( benchSuite
  ) where

import Gauge.Main
import Data.Poly.Sparse
import qualified Data.Vector.Unboxed as U

benchSuite :: Benchmark
benchSuite = bgroup "sparse" $ concat
  [ map benchAdd      $ zip3 tabs vecs2 vecs3
  , map benchMul      $ take 2 $ zip3 tabs vecs2 vecs3
  , map benchEval     $ zip tabs vecs2
  , map benchDeriv    $ zip tabs vecs2
  , map benchIntegral $ zip tabs vecs2'
  ]

tabs :: [Int]
tabs = [10, 100, 1000, 10000]

vecs2 :: [UPoly Int]
vecs2 = flip map tabs $
  \n -> toPoly $ U.generate n (\i -> (fromIntegral i ^ 2, i * 2))

vecs2' :: [UPoly Double]
vecs2' = flip map tabs $
  \n -> toPoly $ U.generate n (\i -> (fromIntegral i ^ 2, fromIntegral i * 2))

vecs3 :: [UPoly Int]
vecs3 = flip map tabs $
  \n -> toPoly $ U.generate n (\i -> (fromIntegral i ^ 3, i * 3))

benchAdd :: (Int, UPoly Int, UPoly Int) -> Benchmark
benchAdd (k, xs, ys) = bench ("add/" ++ show k) $ nf (doBinOp (+) xs) ys

benchMul :: (Int, UPoly Int, UPoly Int) -> Benchmark
benchMul (k, xs, ys) = bench ("mul/" ++ show k) $ nf (doBinOp (*) xs) ys

benchEval :: (Int, UPoly Int) -> Benchmark
benchEval (k, xs) = bench ("eval/" ++ show k) $ nf doEval xs

benchDeriv :: (Int, UPoly Int) -> Benchmark
benchDeriv (k, xs) = bench ("deriv/" ++ show k) $ nf doDeriv xs

benchIntegral :: (Int, UPoly Double) -> Benchmark
benchIntegral (k, xs) = bench ("integral/" ++ show k) $ nf doIntegral xs

doBinOp :: (forall a. Num a => a -> a -> a) -> UPoly Int -> UPoly Int -> Int
doBinOp op xs ys = U.foldl' (\acc (_, x) -> acc + x) 0 zs
  where
    zs = unPoly $ xs `op` ys
{-# INLINE doBinOp #-}

doEval :: UPoly Int -> Int
doEval xs = eval xs (U.length (unPoly xs))

doDeriv :: UPoly Int -> Int
doDeriv xs = U.foldl' (\acc (_, x) -> acc + x) 0 zs
  where
    zs = unPoly $ deriv xs

doIntegral :: UPoly Double -> Double
doIntegral xs = U.foldl' (\acc (_, x) -> acc + x) 0 zs
  where
    zs = unPoly $ integral xs
