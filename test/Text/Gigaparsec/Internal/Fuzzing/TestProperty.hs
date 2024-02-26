module Text.Gigaparsec.Internal.Fuzzing.TestProperty (tests) where

import           Test.Tasty.HUnit

import           Test.Tasty
import           Test.Tasty.QuickCheck
import           Text.Gigaparsec.Internal.Fuzzing.CombinatorAST
import           Text.Gigaparsec.Internal.Fuzzing.Compiler

main :: IO ()
main = do
  structure <- generate (arbitrary :: Gen (Combinator String))
  parserAST <- generate (traverseCombinator structure)
  print structure

-- Just wrapping main for now
tests :: TestTree
tests = testCase "Test" $ do
    main
    assertBool "this should always be true" True
