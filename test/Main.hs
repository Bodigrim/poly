module Main where

import Test.Tasty

import qualified Dense as Dense
import qualified Sparse as Sparse

main :: IO ()
main = defaultMain $ testGroup "All"
    [ Dense.testSuite
    , Sparse.testSuite
    ]
