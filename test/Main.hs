module Main where

import Test.Tasty

import qualified Dense as Dense
import qualified DenseLaurent as DenseLaurent
import qualified Orthogonal as Orthogonal
import qualified Sparse as Sparse

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Dense.testSuite
    , DenseLaurent.testSuite
    , Sparse.testSuite
    , Orthogonal.testSuite
    ]
