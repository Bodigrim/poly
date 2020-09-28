module Main where

import Test.Tasty

import qualified Dense
import qualified DenseLaurent
import qualified DFT
import qualified Multi
import qualified MultiLaurent
import qualified Orthogonal
import qualified Sparse
import qualified SparseLaurent

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Dense.testSuite
    , DenseLaurent.testSuite
    , DFT.testSuite
    , Sparse.testSuite
    , SparseLaurent.testSuite
    , Multi.testSuite
    , MultiLaurent.testSuite
    , Orthogonal.testSuite
    ]
