-- | This module contains queries that operate on a PackageDescription. It provides a function
-- to extract all targets along with their dependencies.
module Distribution.Client.Dynamic.PackageDescription
  ( Target(..)
  , TargetInfo(..)
  , PackageDescription()
  , targets
  , targetName, isLibrary, isExecutable, isTest, isBench
  ) where

import Control.Applicative
import Data.Version
import Distribution.Client.Dynamic.Query
import Language.Haskell.Exts.Syntax
import Language.Haskell.Generate

-- Type tags that we can use to make sure we don't accidently generate code that
-- use a function for a PackageDescription on a BuildInfo value.

-- | A package description type. This type has no constructors, and is only used 
-- for type-safety purposes.
data PackageDescription
data BuildInfo
data CompilerFlavor
data Extension
data Dependency
data ModuleName
instance Eq CompilerFlavor where _ == _ = undefined

-- | The specific information on a target, depending on the target type.
-- Libraries don't have a name, they are always named after the package, but other types do
data TargetInfo = Library [String] -- ^ contains the names of exposed modules
  | Executable String FilePath -- ^ contains the name of the executable and the path to the Main module
  | TestSuite String (Maybe FilePath) -- ^ contains the name of the test suite and the path to the Main module, for stdio tests
  | BenchSuite String  -- ^ contains the name of the benchmark
  deriving (Show, Eq, Read, Ord)

-- | A target is a single Library, an Executable, a TestSuite or a Benchmark.
data Target = Target
  { -- | The specific info of the target
    info         :: TargetInfo

    -- | All dependencies of the target, with their versions. If the version is not resolved yet, it'll be Nothing. 
    -- That only happens when the target is not enabled, though.
  , dependencies :: [(String, Maybe Version)]

    -- | Directories where to look for source files. 
  , sourceDirs   :: [FilePath]

    -- | Directories where to look for header files.
  , includeDirs  :: [FilePath]

    -- | Additional options to pass to GHC when compiling source files.
  , ghcOptions   :: [String]

    -- | Additional options to pass to CPP preprocessor when compiling source files.
  , cppOptions   :: [String]

    -- | The extensions to enable/disable. The elements are like GHC's -X flags, a disabled extension 
    -- is represented as the extension name prefixed by 'No'.
    -- Example value: extensions = ["ScopedTypeVariables", "NoMultiParamTypeClasses"]
  , extensions   :: [String]

    -- | The 'buildable' field in the package description.
  , buildable    :: Bool

    -- | other modules included in the target
  , otherModules :: [String]

    -- | Whether this target was enabled or not. This only matters for Benchmarks or Tests, Executables and Libraries are always enabled.
  , enabled      :: Bool
  

  } deriving (Show, Eq, Read)


-- | return the target name, or the empty string for the library target
targetName :: Target -> String
targetName t = case info t of
  (Library _)      -> ""
  (Executable n _) -> n
  (TestSuite  n _) -> n
  (BenchSuite n)   -> n

-- | is the target the library?
isLibrary :: Target -> Bool
isLibrary t = case info t of
  (Library _) -> True
  _           -> False

-- | is the target an executable?
isExecutable :: Target -> Bool
isExecutable t = case info t of
  (Executable _ _) -> True
  _                -> False

-- | is the target a test suite?
isTest :: Target -> Bool
isTest t = case info t of
  (TestSuite _ _) -> True
  _               -> False

-- | is the target a benchmark?
isBench :: Target -> Bool
isBench t = case info t of
  (BenchSuite _) -> True
  _              -> False

buildable' :: Selector BuildInfo Bool
buildable' = selector $ const $ useValue "Distribution.PackageDescription" $ Ident "buildable"

hsSourceDirs' :: Selector BuildInfo [FilePath]
hsSourceDirs' = selector $ const $ useValue "Distribution.PackageDescription" $ Ident "hsSourceDirs"

-- | The include search path of a buildInfo. Same as the 'includeDir' field in Cabal's BuildInfo.
includeDirs' :: Selector BuildInfo [FilePath]
includeDirs' = selector $ const $ useValue "Distribution.PackageDescription" $ Ident "includeDirs"

-- | Get the names of the extensions to enable/disable for all source files in the package. If an extension should
-- be disabled, it's name is prefixed by 'No'. This corresponds to the names of -X flags to pass to GHC.
extensions' :: Selector BuildInfo [String]
extensions' = selector $ const $ expr $ \bi -> applyE2 map' display' $ applyE concat' $ expr $ map (<>$ bi) [defaultExtensions', oldExtensions', otherExtensions']
  where display' :: ExpG (Extension -> String)
        display' = useValue "Distribution.Text" $ Ident "display"
        
        defaultExtensions', oldExtensions',otherExtensions' :: ExpG (BuildInfo -> [Extension])
        defaultExtensions' = useValue "Distribution.PackageDescription" $ Ident "defaultExtensions"
        oldExtensions' = useValue "Distribution.PackageDescription" $ Ident "oldExtensions"
        otherExtensions' = useValue "Distribution.PackageDescription" $ Ident "otherExtensions"

-- | Get the options to pass to GHC for a given BuildInfo.
ghcOptions' :: Selector BuildInfo [String]
ghcOptions' = selector $ const $ concat' <>. applyE map' snd' <>. applyE filter' (applyE equal' ghc <>. fst') <>. options'
  where options' :: ExpG (BuildInfo -> [(CompilerFlavor, [String])])
        options' = useValue "Distribution.PackageDescription" $ Ident "options"

        ghc :: ExpG CompilerFlavor
        ghc = useValue "Distribution.Compiler" $ Ident "GHC"

-- | Get the options to pass to GHC for a given BuildInfo.
cppOptions' :: Selector BuildInfo [String]
cppOptions' = selector $ const options'
  where options' :: ExpG (BuildInfo -> [String])
        options' = useValue "Distribution.PackageDescription" $ Ident "cppOptions"

-- | Get the non exposed modules.
otherModules' :: Selector BuildInfo [String]
otherModules' = selector $ const $ applyE map' display' <>. mods'
  where display' :: ExpG (Distribution.Client.Dynamic.PackageDescription.ModuleName -> String)
        display' = useValue "Distribution.Text" $ Ident "display"
        mods' :: ExpG (BuildInfo -> [Distribution.Client.Dynamic.PackageDescription.ModuleName])
        mods' = useValue "Distribution.PackageDescription" $ Ident "otherModules"


-- | Get the dependencies of the target and the version of the dependency if possible. If the dependencies version
-- is not a specific version (this only happens when the target is not enabled), return Nothing.
dependencies' ::  Selector BuildInfo [(String, Maybe Version)]
dependencies' = selector $ const $ applyE map' serializeDep <>. targetBuildDepends'
  where serializeDep :: ExpG (Dependency -> (String, Maybe Version))
        serializeDep = expr $ \dep -> do
          dependency  <- useCon "Distribution.Package" $ Ident "Dependency"
          packageName <- useCon "Distribution.Package" $ Ident "PackageName"
          let isSpecificVersion = useValue "Distribution.Version" $ Ident "isSpecificVersion"
          nameVar     <- newName "name"
          versionVar  <- newName "version"
          caseE dep
            [ ( PApp dependency [PApp packageName [PVar nameVar], PVar versionVar], 
                  tuple2 <>$ useVar nameVar <>$ applyE isSpecificVersion (useVar versionVar)
              )
            ]

        targetBuildDepends' :: ExpG (BuildInfo -> [Dependency])
        targetBuildDepends' = useValue "Distribution.PackageDescription" $ Ident "targetBuildDepends"

-- | Construct a 'Target' from a buildInfo, a targetName and a Bool that is True if the target is enabled, false otherwise.
buildInfoTarget :: Query BuildInfo (TargetInfo -> Bool -> Target)
buildInfoTarget = (\d src inc opts copts exts ba oths n-> Target n d src inc opts copts exts ba oths)
                 <$> query dependencies' 
                 <*> query hsSourceDirs' 
                 <*> query includeDirs' 
                 <*> query ghcOptions'
                 <*> query cppOptions'
                 <*> query extensions'
                 <*> query buildable'
                 <*> query otherModules'

-- | Get the buildInfo of the library in the package, and its exposed modules. If there is no library in the package, 
-- return the empty list.
library' :: ExpG (PackageDescription -> [([String],BuildInfo)])
library' = applyE2 maybe' (returnE $ List []) serialize' <>. useValue "Distribution.PackageDescription" (Ident "library")
  where serialize' = expr $ \lib -> applyE2 cons (tuple2 <>$ applyE modNames' lib <>$ applyE buildInfo' lib) (returnE $ List [])
        modNames'=applyE map' display' <>. mods'
        display' = useValue "Distribution.Text" $ Ident "display"
        mods'   = useValue "Distribution.PackageDescription" $ Ident "exposedModules"
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "libBuildInfo"

-- | Get the buildInfo, the name and Main module path of each executable in the package.
executables' :: ExpG (PackageDescription -> [((String,FilePath), BuildInfo)])
executables'= applyE map' serialize' <>. useValue "Distribution.PackageDescription" (Ident "executables")
  where serialize' = expr $ \exe -> tuple2 <>$ applyE exeInfo exe <>$ applyE buildInfo' exe
        exeInfo= expr $ \exe -> tuple2 <>$ applyE exeName' exe <>$ applyE modulePath' exe 
        exeName'   = useValue "Distribution.PackageDescription" $ Ident "exeName"
        modulePath'= useValue "Distribution.PackageDescription" $ Ident "modulePath"
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "buildInfo"


-- | Get the name, whether the target is enabled or not, possibly the main module path and the buildInfo of each testSuite in the package.
tests' :: ExpG (PackageDescription -> [((String, Bool,Maybe FilePath), BuildInfo)])
tests' = applyE map' serialize' <>. useValue "Distribution.PackageDescription" (Ident "testSuites")
  where serialize' = expr $ \test -> tuple2 
                                    <>$ applyE3 tuple3 (testName' <>$ test) (testEnabled' <>$ test) (testPath <>. testInterface' <>$ test) 
                                    <>$ applyE buildInfo' test
        testName'   = useValue "Distribution.PackageDescription" $ Ident "testName"
        testEnabled' = useValue "Distribution.PackageDescription" $ Ident "testEnabled"
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "testBuildInfo"
        testInterface' = useValue "Distribution.PackageDescription" $ Ident "testInterface"
        testPath=expr $ \ti->do
                v10  <- useCon "Distribution.PackageDescription" $ Ident "TestSuiteExeV10"
                versionVar  <- newName "version"
                fpVar  <- newName "filepath"                
                caseE ti 
                        [(PApp v10 [PVar versionVar,PVar fpVar],applyE just' (useVar fpVar))
                        ,(PWildCard,nothing')
                        ] 

-- | Get the name, whether it's enabled or not and the buildInfo of each benchmark in the package.
benchmarks' :: ExpG (PackageDescription -> [((String, Bool), BuildInfo)])
benchmarks' = applyE map' serialize' <>. useValue "Distribution.PackageDescription" (Ident "benchmarks")
  where serialize' = expr $ \bench -> tuple2 
                                     <>$ applyE2 tuple2 (benchName' <>$ bench) (benchEnabled' <>$ bench)
                                     <>$ applyE buildInfo' bench
        benchName'   = useValue "Distribution.PackageDescription" $ Ident "benchmarkName"
        benchEnabled' = useValue "Distribution.PackageDescription" $ Ident "benchmarkEnabled"
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "benchmarkBuildInfo"

-- | Get the name of all targets and whether they are enabled (second field True) or not. 
-- The resulting list is in the same order and has the same length as the list returned
-- by buildInfos.
targetInfos :: Query PackageDescription [(TargetInfo, Bool)]
targetInfos = build <$> query libMods <*> query exeNames <*> query testInfo <*> query benchInfo
  where libMods :: Selector PackageDescription [[String]]
        libMods =  selector $ const $ applyE map' fst' <>. library'

        exeNames :: Selector PackageDescription [(String,FilePath)]
        exeNames = selector $ const $ applyE map' fst' <>. executables'

        testInfo :: Selector PackageDescription [(String, Bool,Maybe FilePath)]
        testInfo = selector $ const $ applyE map' fst' <>. tests'

        benchInfo :: Selector PackageDescription [(String, Bool)]
        benchInfo = selector $ const $ applyE map' fst' <>. benchmarks'

        build lib exe test bench = concat
          [ [ (Library    x   , True) | x        <- lib   ]
          , [ (Executable x mp, True) | (x,mp)   <- exe   ]
          , [ (TestSuite  x mp, e   ) | (x,e,mp) <- test  ]
          , [ (BenchSuite x   , e   ) | (x,e)    <- bench ]
          ]

-- | Get the BuildInfo of all targets, even for disable or not buildable targets.
buildInfos :: Selector PackageDescription [BuildInfo]
buildInfos = selector $ const $ expr $ \bi -> applyE concat' $ expr $ map (<>$ bi) [libraryBI, exesBI, testsBI, benchsBI]
  where libraryBI :: ExpG (PackageDescription -> [BuildInfo])
        libraryBI = applyE map' snd' <>. library'
        
        exesBI    :: ExpG (PackageDescription -> [BuildInfo])
        exesBI    = applyE map' snd' <>. executables'
                     
        testsBI   :: ExpG (PackageDescription -> [BuildInfo])
        testsBI   = applyE map' snd' <>. tests'

        benchsBI  :: ExpG (PackageDescription -> [BuildInfo])
        benchsBI  = applyE map' snd' <>. benchmarks'

-- | Query the available targets. This will return all targets, even disabled ones. 
-- If a package is disabled or not buildable, it's possible that not all dependencies have versions, some can be Nothing.
targets :: Query PackageDescription [Target]
targets = zipWith uncurry <$> on buildInfos (fmapQ buildInfoTarget) <*> targetInfos
