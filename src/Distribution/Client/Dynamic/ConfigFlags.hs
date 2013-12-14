{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Distribution.Client.Dynamic.ConfigFlags 
  ( ConfigFlags()
  , ProgramDB()
  , lookupProgram, programPath
  , configPrograms, configHcPath, configDistPref
  ) where

import Control.Category
import Data.Maybe
import Distribution.Client.Dynamic.Query
import Language.Haskell.Exts.Syntax
import Language.Haskell.Generate
import Prelude hiding ((.), id)

data ConfigFlags
data Flag a
data Program
data ProgramDB
data ConfiguredProgram

flagMaybe :: Selector (Flag a) (Maybe a)
flagMaybe = selector $ const $ useValue "Distribution.Simple.Setup" $ Ident "flagToMaybe"

programPath :: Selector ConfiguredProgram FilePath
programPath = selector $ const $ useValue "Distribution.Simple.Program.Types" $ Ident "programPath"

lookupProgram :: String -> Selector ProgramDB (Maybe ConfiguredProgram)
lookupProgram prog = selector $ const $ expr $ \db -> applyE2 bind' (applyE2 lookupKnownProgram' (expr prog) db) $ expr $ \prog' -> applyE2 lookupProgram' prog' db
  where lookupKnownProgram' :: ExpG (String -> ProgramDB -> Maybe Program)
        lookupKnownProgram' = useValue "Distribution.Simple.Program" $ Ident "lookupKnownProgram"

        lookupProgram'      :: ExpG (Program -> ProgramDB -> Maybe ConfiguredProgram)
        lookupProgram'      = useValue "Distribution.Simple.Program" $ Ident "lookupProgram"

configPrograms :: Selector ConfigFlags ProgramDB
configPrograms = selector $ const $ useValue "Distribution.Simple.Setup" $ Ident "configPrograms"

configHcPath :: Selector ConfigFlags (Maybe FilePath)
configHcPath = flagMaybe . selector (const $ useValue "Distribution.Simple.Setup" $ Ident "configHcPath")

configDistPref :: Selector ConfigFlags (Maybe FilePath)
configDistPref = flagMaybe . selector (const $ useValue "Distribution.Simple.Setup" $ Ident "configDistPref")
