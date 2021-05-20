{-# language BlockArguments #-}

module Main ( main ) where

import Control.Monad ( guard, mzero, msum )
import System.Directory ( doesDirectoryExist, setCurrentDirectory )
import qualified Test.Tasty

import qualified Gerber.Grammar.Tests

main :: IO ()
main = do
  changeToParentOfTestDataDirectory
  Test.Tasty.defaultMain tests


changeToParentOfTestDataDirectory :: IO ()
changeToParentOfTestDataDirectory =
  msum
    -- run from cabal project directory
    [ doesDirectoryExist "./test-data" >>= guard
    -- run from parent directory
    , do
        doesDirectoryExist "./gerber" >>= guard
        setCurrentDirectory "./gerber"
        doesDirectoryExist "./test-data" >>= guard
    -- run from some other directory
    , putStrLn "Could not find `test-data` directory" >> mzero
    ]


tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup
    "gerber tests"
    [ Gerber.Grammar.Tests.tests
    ]
