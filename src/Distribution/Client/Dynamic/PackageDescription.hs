-- | This module contains queries that operate on a PackageDescription. It provides a function
-- to extract all targets along with their dependencies.
module Distribution.Client.Dynamic.PackageDescription
  ( Target(..)
  , TargetName(..)
  , PackageDescription()
  , targets
  ) where

import Control.Applicative
import Data.Version
import Distribution.Client.Dynamic.Query
import Language.Haskell.Exts.Syntax
import Language.Haskell.Generate

-- Type tags that we can use to make sure we don't accidently generate code that
-- use a function for a PackageDescription on a BuildInfo value.
data PackageDescription
data BuildInfo
data CompilerFlavor
data Extension
data Dependency
instance Eq CompilerFlavor where _ == _ = undefined

-- | The name of a target. Libraries don't have a name, they are always named after the package.
data TargetName = Library | Executable String | TestSuite String | BenchSuite String deriving (Show, Eq, Read, Ord)

-- | A target is a single Library, an Excutable, a TestSuite or a Benchmark.
data Target = Target
  { -- | The name of the target
    name         :: TargetName

    -- | All dependencies of the target, with their versions. If the version is not resolved yet, it'll be Nothing. 
    -- That only happens when the target is not enabled, though.
  , dependencies :: [(String, Maybe Version)]

    -- | Directories where to look for source files. 
  , sourceDirs   :: [FilePath]

    -- | Directories where to look for header files.
  , includeDirs  :: [FilePath]

    -- | Additional options to pass to GHC when compiling source files.
  , ghcOptions   :: [String]

    -- | The extensions to enable/disable. The elements are like GHC's -X flags, a disabled extension 
    -- is represented as the extension name prefixed by 'No'.
    -- Example value: extensions = ["ScopedTypeVariables", "NoMultiParamTypeClasses"]
  , extensions   :: [String]

    -- | The 'buildable' field in the package description.
  , buildable    :: Bool

    -- | Whether this target was enabled or not. This only matters for Benchmarks or Tests, Executables and Libraries are always enabled.
  , enabled      :: Bool
  } deriving (Show, Eq, Read)

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
extensions' = selector $ const $ expr $ \bi -> applyE2 map' display' $ append' <>$ applyE defaultExtensions' bi <>$ applyE oldExtensions' bi
  where display' :: ExpG (Extension -> String)
        display' = useValue "Distribution.Text" $ Ident "display"
        
        defaultExtensions', oldExtensions' :: ExpG (BuildInfo -> [Extension])
        defaultExtensions' = useValue "Distribution.PackageDescription" $ Ident "defaultExtensions"
        oldExtensions' = useValue "Distribution.PackageDescription" $ Ident "oldExtensions"

-- | Get the options to pass to GHC for a given BuildInfo.
ghcOptions' :: Selector BuildInfo [FilePath]
ghcOptions' = selector $ const $ concat' <>. applyE map' snd' <>. applyE filter' (applyE equal' ghc <>. fst') <>. options'
  where options' :: ExpG (BuildInfo -> [(CompilerFlavor, [String])])
        options' = useValue "Distribution.PackageDescription" $ Ident "options"

        ghc :: ExpG CompilerFlavor
        ghc = useValue "Distribution.Compiler" $ Ident "GHC"

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
buildInfoTarget :: Query BuildInfo (TargetName -> Bool -> Target)
buildInfoTarget = (\d src inc opts exts ba n -> Target n d src inc opts exts ba)
                 <$> query dependencies' 
                 <*> query hsSourceDirs' 
                 <*> query includeDirs' 
                 <*> query ghcOptions'
                 <*> query extensions'
                 <*> query buildable'

-- | Get the buildInfo of the library in the package. If there is no library in the package, 
-- return the empty list.
library' :: ExpG (PackageDescription -> [BuildInfo])
library' = applyE2 maybe' (returnE $ List []) serialize' <>. useValue "Distribution.PackageDescription" (Ident "library")
  where serialize' = expr $ \lib -> expr [applyE buildInfo' lib]
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "libBuildInfo"

-- | Get the buildInfo and the name of each executable in the package.
executables' :: ExpG (PackageDescription -> [(String, BuildInfo)])
executables'= applyE map' serialize' <>. useValue "Distribution.PackageDescription" (Ident "executables")
  where serialize' = expr $ \exe -> tuple2 <>$ applyE exeName' exe <>$ applyE buildInfo' exe
        exeName'   = useValue "Distribution.PackageDescription" $ Ident "exeName"
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "buildInfo"

-- | Get the name, whether the target is enabled or not and the buildInfo of each testSuite in the package.
tests' :: ExpG (PackageDescription -> [((String, Bool), BuildInfo)])
tests' = applyE map' serialize' <>. useValue "Distribution.PackageDescription" (Ident "testSuites")
  where serialize' = expr $ \test -> tuple2 
                                    <>$ applyE2 tuple2 (testName' <>$ test) (testEnabled' <>$ test) 
                                    <>$ applyE buildInfo' test
        testName'   = useValue "Distribution.PackageDescription" $ Ident "testName"
        testEnabled' = useValue "Distribution.PackageDescription" $ Ident "testEnabled"
        buildInfo' = useValue "Distribution.PackageDescription" $ Ident "testBuildInfo"

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
targetInfos :: Query PackageDescription [(TargetName, Bool)]
targetInfos = build <$> query hasLib <*> query exeNames <*> query testInfo <*> query benchInfo
  where hasLib :: Selector PackageDescription Bool
        hasLib = selector $ const $ not' <>. null' <>. library'

        exeNames :: Selector PackageDescription [String]
        exeNames = selector $ const $ applyE map' fst' <>. executables'

        testInfo :: Selector PackageDescription [(String, Bool)]
        testInfo = selector $ const $ applyE map' fst' <>. tests'

        benchInfo :: Selector PackageDescription [(String, Bool)]
        benchInfo = selector $ const $ applyE map' fst' <>. benchmarks'

        build lib exe test bench = concat
          [ [ (Library      , True) | lib           ]
          , [ (Executable x , True) | x <- exe       ]
          , [ (TestSuite  x , e)    | (x,e) <- test  ]
          , [ (BenchSuite x , e)    | (x,e) <- bench ]
          ]

-- | Get the BuildInfo of all targets, even for disable or not buildable targets.
buildInfos :: Selector PackageDescription [BuildInfo]
buildInfos = selector $ const $ expr $ \bi -> applyE concat' $ expr $ map (<>$ bi) [libraryBI, exesBI, testsBI, benchsBI]
  where libraryBI :: ExpG (PackageDescription -> [BuildInfo])
        libraryBI = library'
        
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
