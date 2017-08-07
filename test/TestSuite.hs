{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Protolude

import qualified Data.Text as T

import Path ((</>), reldir, toFilePath)
import Path.IO (getCurrentDir, listDirRecur)

import Test.Tasty (TestTree, defaultMainWithIngredients, testGroup)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.Runners (listingTests, consoleTestReporter)
import Test.Tasty.Ingredients.Rerun (rerunningTests)

import Projectile (getProjectRootDir)

main :: IO ()
main =
  defaultMainWithIngredients
    [ rerunningTests [listingTests, consoleTestReporter] ]
    (testGroup "all-tests" tests)

tests :: [TestTree]
tests =
  [
    testGroup "getProjectRootDir" getProjectRootDirTests
  ]

-- scTests :: [TestTree]
-- scTests =
--   [ testProperty "inc == succ" prop_succ
--   , testProperty "inc . negate == negate . pred" prop_pred
--   ]

getProjectRootDirTests :: [TestTree]
getProjectRootDirTests =
  [
    testCase "every sub-folder file marker" $ do
      root <- getCurrentDir
      let
        rootDirProject =
          root </> [reldir|test/fixtures/recurring-mark-file|]

      (dirs, _files) <- listDirRecur rootDirProject
      forM_ dirs $ \dir -> do
        unless (".svn/" `T.isSuffixOf` T.pack (toFilePath dir)) $ do
          rootDirProject' <- getProjectRootDir dir
          assertEqual ("doesn't return the root of the project: " <> show dir)
                      rootDirProject
                      rootDirProject'

  , testCase "top level folder marker" $ do
      root <- getCurrentDir
      let
        rootDirProject =
          root </> [reldir|test/fixtures/top-mark-file|]

      (dirs, _files) <- listDirRecur rootDirProject
      forM_ dirs $ \dir -> do
        rootDirProject' <- getProjectRootDir dir
        assertEqual ("doesn't return the root of the project: " <> show dir)
                    rootDirProject
                    rootDirProject'

  ]
