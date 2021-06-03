{-# language BlockArguments #-}

module Main ( main ) where

import Control.Monad ( guard, mzero, msum )
import System.Directory ( doesDirectoryExist, setCurrentDirectory )
import qualified Test.Tasty

import qualified Gerber.Diagrams.Tests

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
        doesDirectoryExist "./gerber-diagrams" >>= guard
        setCurrentDirectory "./gerber-diagrams"
        doesDirectoryExist "./test-data" >>= guard
    -- run from some other directory
    , putStrLn "Could not find `test-data` directory" >> mzero
    ]


tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup
    "gerber tests"
    [ Gerber.Diagrams.Tests.tests
    ]
