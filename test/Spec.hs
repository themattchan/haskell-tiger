module Spec where

import System.Directory
import Test.Tasty
import Test.Tasty.HUnit


main :: IO ()
main = defaultMain $ do
  testParser

sampleProgs :: IO [FilePath]
sampleProgs = listDirectory "testcases"

-- testParser :: TestTree
-- testParser = traverse_ $ \file ->
--   testCase file (
