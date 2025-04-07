{-# LANGUAGE CPP #-}

module Main where

import Test.Tasty

import qualified Dense
import qualified DenseLaurent
import qualified DFT
import qualified Orthogonal
import qualified Interpolation
#ifdef SupportSparse
import qualified Multi
import qualified MultiLaurent
import qualified Sparse
import qualified SparseLaurent
#endif

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Dense.testSuite
    , DenseLaurent.testSuite
    , DFT.testSuite
    , Interpolation.testSuite
    , Orthogonal.testSuite
#ifdef SupportSparse
    , Sparse.testSuite
    , SparseLaurent.testSuite
    , Multi.testSuite
    , MultiLaurent.testSuite
#endif
    ]
