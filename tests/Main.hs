{-# LANGUAGE TemplateHaskell #-}
import Data.List
import Distribution.Client.Dynamic
import Test.HUnit
import Test.Tasty.HUnit
import Test.Tasty.TH

case_targets :: Assertion
case_targets = do
  tgs <- runQuery (on localPkgDesc targets) "dist/setup-config"
  assertEqual "target names" (sort [Library, TestSuite "dynamic-cabal-tests", TestSuite "doctests"]) (sort $ map name tgs) 
  assertEqual "source directories" (sort $ map sourceDirs tgs) $ sort $ map return ["src", "tests", "tests"]
  assertBool "ghc options" $ all (elem "-Wall" . ghcOptions)  tgs
  assertBool "no extensions" $ all (null . extensions) tgs
  assertBool  "everything buildable" $ all buildable tgs

case_packageDBs :: Assertion
case_packageDBs = do
  dbs <- runQuery packageDBs "dist/setup-config"
  length dbs `seq` return ()

main :: IO ()
main = $(defaultMainGenerator)
