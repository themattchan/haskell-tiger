module Spec where

import Data.Either
import Data.Traversable

import System.Directory
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BL

import Language.Tiger.Parser

main :: IO ()
main = defaultMain $ do
  progs <- sampleProgs
  testGroup "parser" (map testParser progs)

sampleProgs :: IO [FilePath]
sampleProgs = listDirectory "testcases"

testParser :: FilePath -> TestTree
testParser file = testCase file $ do
  src <- BL.readFile file
  isRight (parseProgram src) @?= True
