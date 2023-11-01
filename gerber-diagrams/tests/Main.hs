{-# LANGUAGE BlockArguments #-}

module Main (main) where

-- base
import Control.Monad (guard, msum, mzero)

-- directory
import System.Directory (doesDirectoryExist, setCurrentDirectory)

-- gerber-diagrams
import qualified Gerber.Diagrams.Tests

-- tasty
import qualified Test.Tasty


main :: IO ()
main = do
  changeToParentOfTestDataDirectory
  Test.Tasty.defaultMain tests


changeToParentOfTestDataDirectory :: IO ()
changeToParentOfTestDataDirectory =
  msum
    -- run from cabal project directory
    [ doesDirectoryExist "./test-data" >>= guard
    , -- run from parent directory
      do
        doesDirectoryExist "./gerber-diagrams" >>= guard
        setCurrentDirectory "./gerber-diagrams"
        doesDirectoryExist "./test-data" >>= guard
    , -- run from some other directory
      putStrLn "Could not find `test-data` directory" >> mzero
    ]


tests :: Test.Tasty.TestTree
tests =
  Test.Tasty.testGroup
    "gerber tests"
    [ Gerber.Diagrams.Tests.tests
    ]
