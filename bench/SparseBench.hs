{-# LANGUAGE DataKinds  #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module SparseBench
  ( benchSuite
  ) where

import Gauge.Main
import Data.List
import Data.Poly.Sparse
import qualified Data.Vector.Unboxed as U

import qualified Math.Algebra.Polynomial.Univariate as AP
import qualified Math.Algebra.Polynomial.Class as AP

benchSuite :: Benchmark
benchSuite = bgroup "sparse" $ concat
  [ zipWith3 benchAdd tabs vecs2 vecs3
  , zipWith3 benchAddAP tabs aps2 aps3
  , take 3
  $ zipWith3 benchMul tabs vecs2 vecs3
  , take 3
  $ zipWith3 benchMulAP tabs aps2 aps3
  , zipWith benchEval tabs vecs2
  , zipWith benchDeriv tabs vecs2
  , zipWith benchIntegral tabs vecs2'
  ]

tabs :: [Int]
tabs = [10, 100, 1000, 10000]

vecs2 :: [UPoly Int]
vecs2 = flip map tabs $
  \n -> toPoly $ U.generate n (\i -> (fromIntegral i ^ 2, i * 2))

aps2 :: [AP.Univariate Int "x"]
aps2 = flip map tabs $
  \n -> AP.fromListP (map (\i -> (AP.U $ fromIntegral i ^ 2, i * 2)) [0..n-1])

vecs2' :: [UPoly Double]
vecs2' = flip map tabs $
  \n -> toPoly $ U.generate n (\i -> (fromIntegral i ^ 2, fromIntegral i * 2))

vecs3 :: [UPoly Int]
vecs3 = flip map tabs $
  \n -> toPoly $ U.generate n (\i -> (fromIntegral i ^ 3, i * 3))

aps3 :: [AP.Univariate Int "x"]
aps3 = flip map tabs $
  \n -> AP.fromListP (map (\i -> (AP.U $ fromIntegral i ^ 3, i * 3)) [0..n-1])

benchAdd :: Int -> UPoly Int -> UPoly Int -> Benchmark
benchAdd k xs ys = bench ("add/" ++ show k) $ nf (doBinOp (+) xs) ys

benchAddAP :: Int -> AP.Univariate Int "x" -> AP.Univariate Int "x" -> Benchmark
benchAddAP k xs ys = bench ("addAP/" ++ show k) $ nf (doBinOpAP (+) xs) ys

benchMul :: Int -> UPoly Int -> UPoly Int -> Benchmark
benchMul k xs ys = bench ("mul/" ++ show k) $ nf (doBinOp (*) xs) ys

benchMulAP :: Int -> AP.Univariate Int "x" -> AP.Univariate Int "x" -> Benchmark
benchMulAP k xs ys = bench ("mulAP/" ++ show k) $ nf (doBinOpAP (*) xs) ys

benchEval :: Int -> UPoly Int -> Benchmark
benchEval k xs = bench ("eval/" ++ show k) $ nf doEval xs

benchDeriv :: Int -> UPoly Int -> Benchmark
benchDeriv k xs = bench ("deriv/" ++ show k) $ nf doDeriv xs

benchIntegral :: Int -> UPoly Double -> Benchmark
benchIntegral k xs = bench ("integral/" ++ show k) $ nf doIntegral xs

doBinOp :: (forall a. Num a => a -> a -> a) -> UPoly Int -> UPoly Int -> Int
doBinOp op xs ys = U.foldl' (\acc (_, x) -> acc + x) 0 zs
  where
    zs = unPoly $ xs `op` ys
{-# INLINE doBinOp #-}

doBinOpAP :: (forall a. Num a => a -> a -> a) -> AP.Univariate Int "x" -> AP.Univariate Int "x" -> Int
doBinOpAP op xs ys = foldl' (\acc (_, x) -> acc + x) 0 zs
  where
    zs = AP.toListP $ xs `op` ys
{-# INLINE doBinOpAP #-}

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
