module Main where

import Data.Either
import Data.Traversable

import System.Directory
--import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.ByteString.Lazy as BL

import Language.Tiger.Parser

main :: IO ()
main = withCurrentDirectory "testcases" $ do
  progs <- listDirectory "."

  defaultMain $
    testGroup "parser" (map testParser progs)

sampleProgs :: IO [FilePath]
sampleProgs = listDirectory "testcases"

testParser :: FilePath -> TestTree
testParser file = testCase file $ do
  src <- BL.readFile file
  let p = parseProgram src
  assertBool (either id show p) ((if file `elem` bads then isLeft else isRight) p)
  where
    bads = ["test49.tig"]
