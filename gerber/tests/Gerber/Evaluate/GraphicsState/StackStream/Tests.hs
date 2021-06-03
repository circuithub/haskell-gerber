{-# language NamedFieldPuns #-}
{-# language DuplicateRecordFields #-}

module Gerber.Evaluate.GraphicsState.StackStream.Tests ( tests ) where

-- hedgehog
import Hedgehog ( Gen )
import qualified Hedgehog.Gen as Gen

-- hedgehog-classes
import qualified Hedgehog.Classes as Hedgehog

-- tasty
import Test.Tasty ( TestTree, testGroup )

-- tasty-hedgehog
import Test.Tasty.Hedgehog ( testProperty )

import Gerber.Evaluate.GraphicsState.StackStream (StackStream)
import qualified Gerber.Evaluate.GraphicsState.StackStream as StackStream

tests :: TestTree
tests = testGroup "Gerber.Evaluate.GraphicsState.StackStream.Tests"
  [ testLaws (Hedgehog.semigroupLaws genStackCommand)
  , testLaws (Hedgehog.monoidLaws genStackCommand)
  ]

genStackCommand :: Gen (StackStream Char Char)
genStackCommand = Gen.choice
  [ pure mempty
  , StackStream.push <$> Gen.ascii
  , pure StackStream.pop
  , StackStream.add <$> Gen.ascii
  ]

testLaws :: Hedgehog.Laws -> TestTree
testLaws Hedgehog.Laws{ lawsTypeClass, lawsProperties } =
  testGroup lawsTypeClass (map (uncurry testProperty) lawsProperties )
