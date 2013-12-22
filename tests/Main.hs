{-# LANGUAGE TemplateHaskell #-}
import Distribution.Client.Dynamic
import Test.HUnit
import Test.Tasty.HUnit
import Test.Tasty.TH

case_targets :: Assertion
case_targets = do
  tgs <- runQuery (on localPkgDesc targets) "dist/setup-config"
  assertEqual "target names" (map name tgs) [Library, TestSuite "dynamic-cabal-tests", TestSuite "doctests"]
  assertEqual "source directories" (map sourceDirs tgs) $ map return ["src", "tests", "tests"]
  assertEqual "ghc options" (map ghcOptions $ take 2 tgs) [["-Wall"], ["-Wall"]]
  assertBool "no extensions" $ all (null . extensions) tgs
  assertBool  "everything buildable" $ all buildable tgs

case_packageDBs :: Assertion
case_packageDBs = do
  dbs <- runQuery packageDBs "dist/setup-config"
  length dbs `seq` return ()

main :: IO ()
main = $(defaultMainGenerator)
