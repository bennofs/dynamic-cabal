module Distribution.Client.Dynamic.LocalBuildInfo where

import Control.Applicative
import Distribution.Client.Dynamic.PackageDescription
import Distribution.Client.Dynamic.Query
import Language.Haskell.Exts.Syntax
import Language.Haskell.Generate
import Prelude hiding ((.), id)

-- | A package db is either the user package db (often at @~/.ghc/ghc-....@), the global package
-- or a specific file or directory.
data PackageDB = UserDB | GlobalDB | SpecificDB FilePath deriving (Eq, Ord, Show, Read)

-- | Get the package dbs that ghc will use when compiling this package.
packageDBs :: Query LocalBuildInfo [PackageDB]
packageDBs = map deserialize <$> query packageDBStack
  where packageDBStack' :: ExpG (LocalBuildInfo -> [PackageDB])
        packageDBStack' = useValue "Distribution.Simple.LocalBuildInfo" $ Ident "withPackageDB"

        packageDBStack :: Selector LocalBuildInfo [Either Bool FilePath]
        packageDBStack = selector $ const $ applyE map' (expr serialize') <>. packageDBStack'

        serialize' :: ExpG PackageDB -> ExpG (Either Bool FilePath)
        serialize' db = do
          fileVar <- newName "file_"
          globalDB   <- useCon "Distribution.Simple.Compiler" $ Ident "GlobalPackageDB"
          userDB     <- useCon "Distribution.Simple.Compiler" $ Ident "UserPackageDB"
          specificDB <- useCon "Distribution.Simple.Compiler" $ Ident "SpecificPackageDB"
          caseE db
            [ (PApp globalDB [], left' <>$ true')
            , (PApp userDB []  , left' <>$ false')
            , (PApp specificDB [PVar fileVar], right' <>$ useVar fileVar)
            ]

        deserialize (Left isGlobal) | isGlobal  = GlobalDB
                                    | otherwise = UserDB
        deserialize (Right file) = SpecificDB file


-- | Returns the builddir of a LocalBuildInfo. Often, this will just be "dist".
buildDir :: Selector LocalBuildInfo String
buildDir = selector $ const $ useValue "Distribution.Simple.LocalBuildInfo" $ Ident "buildDir"

-- | Returns the package description included in a local build info.
localPkgDesc :: Selector LocalBuildInfo PackageDescription
localPkgDesc = selector $ const $ useValue "Distribution.Simple.LocalBuildInfo" $ Ident "localPkgDescr"
