{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
import Data.List
import Distribution.Client.Dynamic
import Test.HUnit
import Test.Tasty.HUnit
import Test.Tasty.TH
import qualified Data.Map as DM
import           System.Directory
import Data.Version

case_targets :: Assertion
case_targets = do
  tgs <- runQuery (on localPkgDesc targets) "dist/setup-config"
  assertEqual "target names" (sort [Library ["Distribution.Client.Dynamic"
      ,"Distribution.Client.Dynamic.Query"
      ,"Distribution.Client.Dynamic.LocalBuildInfo"
      ,"Distribution.Client.Dynamic.PackageDescription"], TestSuite "dynamic-cabal-tests" (Just "Main.hs"), TestSuite "doctests" (Just "doctests.hs")]) (sort $ map info tgs) 
  assertEqual "source directories" (sort $ map sourceDirs tgs) $ sort $ map return ["src", "tests", "tests"]
  assertBool "ghc options" $ all (elem "-Wall" . ghcOptions)  tgs
  assertBool "no extensions" $ all (null . extensions) tgs
  assertBool  "everything buildable" $ all buildable tgs

case_packageDBs :: Assertion
case_packageDBs = do
  dbs <- runQuery packageDBs "dist/setup-config"
  length dbs `seq` return ()

-- | test running raw query
case_raw :: Assertion
case_raw = do
  s<-src
  m::DM.Map String [String]<-runRawQuery s "dist/setup-config"
  assertEqual "map size" 3 (DM.size m)
  -- print m

-- | the raw query source code
src :: IO String
src=do
  setupConfig' <- canonicalizePath "dist/setup-config"
  cv<-getCabalVersion setupConfig'
  let optStr=if cv>=Version [1,15,0] []
               then "       let opts=renderGhcOptions ((fst $ head $ readP_to_S  parseVersion  \"7.6.3\") :: Version) $ componentGhcOptions V.silent lbi b clbi \"dist/build\""
               else "       let opts=ghcOptions lbi b clbi fp"
  return $ unlines [
    "module DynamicCabalQuery where"
    ,"import Distribution.PackageDescription"
    ,"import Distribution.Simple.LocalBuildInfo"
    ,"import Data.IORef"
    ,"import qualified Data.Map as DM"
    ,"import qualified Distribution.Verbosity as V"
    ,"import Data.Version (parseVersion)"
    ,"import Text.ParserCombinators.ReadP(readP_to_S)"
    ,if cv>=Version [1,15,0] [] then "import Distribution.Simple.Program.GHC" else ""
    ,"import Distribution.Simple.GHC"
    ,"import Distribution.Version"
    ,"import Control.Monad"
    ,""
    ,"result :: IO (DM.Map String [String])"
    ,"result=do"
    ,"lbi<-liftM (read . Prelude.unlines . drop 1 . lines) $ Prelude.readFile \""++setupConfig' ++"\""
    ,"let pkg=localPkgDescr lbi"
    ,"r<-newIORef DM.empty"
    ,"withComponentsLBI pkg lbi (\\c clbi->do"
    ,"       let b=componentBuildInfo c"
    ,optStr 
    ,"       let n=foldComponent (const \"\") exeName testName benchmarkName c"
    ,"       modifyIORef r (DM.insert n opts)"
    ,"       return ()"
    ,"       )"
    ,"readIORef r" ]



main :: IO ()
main = $(defaultMainGenerator)
