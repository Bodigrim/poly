{-# LANGUAGE CPP        #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Gauge.Main
import qualified DenseBench as Dense
#ifdef SupportSparse
import qualified SparseBench as Sparse
#endif

main :: IO ()
main = defaultMain
  [ Dense.benchSuite
#ifdef SupportSparse
  , Sparse.benchSuite
#endif
  ]
