{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
import           Data.List
import qualified Data.Map as DM
import           Data.Version
import           Distribution.Client.Dynamic
import           System.Directory
import           Test.Tasty.HUnit
import           Test.Tasty.TH
import qualified Config

case_targets :: Assertion
case_targets = do
  tgs <- runQuery (on localPkgDesc targets) "dist/setup-config"
  assertEqual "target names"
    (sort
    [ Library
      [ "Distribution.Client.Dynamic"
      , "Distribution.Client.Dynamic.Query"
      , "Distribution.Client.Dynamic.LocalBuildInfo"
      , "Distribution.Client.Dynamic.PackageDescription"
      ]
    , TestSuite "dynamic-cabal-tests" (Just "Main.hs")
    , TestSuite "doctests" (Just "doctests.hs")
    ])
    (sort $ map info tgs)
  assertEqual "source directories" (sort $ map sourceDirs tgs) $ sort $ map return ["src", "tests", "tests"]
  assertBool "ghc options" $ all (elem "-Wall" . ghcOptions)  tgs
  assertBool "no extensions (except TH)" $ all (not . any ("TemplateHaskell" /=) . extensions) tgs
  assertBool "everything buildable" $ all buildable tgs
  assertBool "no cpp options" $ all (null . cppOptions) tgs

case_packageDBs :: Assertion
case_packageDBs = do
  dbs <- runQuery packageDBs "dist/setup-config"
  length dbs `seq` return ()

-- | test running raw query
case_raw :: Assertion
case_raw = do
  s <- src
  m :: DM.Map String [String] <- runRawQuery s "dist/setup-config"
  assertEqual "number of build targets" 3 (DM.size m)

-- | the raw query source code
src :: IO String
src = do
  distPrefix <- canonicalizePath "dist"
  cv <- getCabalVersion (distPrefix ++ "/setup-config")
  let optStr1 | cv >= Version [1,19,0] [] ="(compiler,_ ,_) <- configure V.silent Nothing Nothing defaultProgramDb"
              | otherwise =""
  let optStr | cv >= Version [1,19,0] [] ="renderGhcOptions compiler $ componentGhcOptions V.silent lbi b clbi " ++ show distPrefix
             | cv >= Version [1,15,0] [] ="renderGhcOptions ((fst $ head $ readP_to_S  parseVersion " ++ show Config.cProjectVersion ++ ") :: Version) $ componentGhcOptions V.silent lbi b clbi \"dist/build\""
             | otherwise                 ="ghcOptions lbi b clbi \"dist/build\""
  return $ unlines [
    "module DynamicCabalQuery where"
    ,"import Distribution.PackageDescription"
    ,"import Distribution.Simple.LocalBuildInfo"
    ,"import Distribution.Simple.Configure (maybeGetPersistBuildConfig)"
    ,"import Data.IORef"
    ,"import qualified Data.Map as DM"
    ,"import qualified Distribution.Verbosity as V"
    ,"import Data.Version (parseVersion)"
    ,"import Text.ParserCombinators.ReadP(readP_to_S)"
    ,if cv >= Version [1,15,0] [] then "import Distribution.Simple.Program.GHC" else ""
    ,"import Distribution.Simple.GHC"
    ,if cv >= Version [1,19,0] [] then "import Distribution.Simple.Program.Db(defaultProgramDb)" else ""
    ,"import Distribution.Version"
    ,"import Control.Monad"
    ,""
    ,"result :: IO (DM.Map String [String])"
    ,"result=do"
    ,"Just lbi<-maybeGetPersistBuildConfig \""++distPrefix ++"\""
    ,"let pkg=localPkgDescr lbi"
    ,"r<-newIORef DM.empty"
    , optStr1
    ,(if cv >= Version [1,18,0] [] then "withAllComponentsInBuildOrder" else "withComponentsLBI") ++ " pkg lbi (\\c clbi->do"
    ,"       let b=foldComponent libBuildInfo buildInfo testBuildInfo benchmarkBuildInfo c"
    ,"       let opts=" ++ optStr ++ "::[String]"
    ,"       let n=foldComponent (const \"\") exeName testName benchmarkName c"
    ,"       modifyIORef r (DM.insert n opts)"
    ,"       return ()"
    ,"       )"
    ,"readIORef r" ]


main :: IO ()
main = $(defaultMainGenerator)
