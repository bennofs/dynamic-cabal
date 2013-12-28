{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}
#endif

-- | Functions for building queries on cabal's setup-config an evaluating them.
module Distribution.Client.Dynamic.Query 
  ( Selector(), selector
  , Query(), query
  , LocalBuildInfo()
  , maybeDefault
  , (>>>=), (=<<<)
  , fmapS
  , fmapQ
  , on
  , runQuery
  , runRawQuery
  , getCabalVersion
  ) where

import           Control.Applicative
import           Control.Category
import qualified Control.Exception as E
import           Control.Monad
import           Data.Dynamic
import           Data.Version
import           Data.Void
import qualified DynFlags
import qualified GHC
import qualified GHC.Paths
import           Language.Haskell.Exts.Syntax
import           Language.Haskell.Generate
import qualified MonadUtils
import           Prelude hiding (id, (.))
import           System.Directory
import           System.FilePath
import           System.IO.Error (isAlreadyExistsError)
import           Text.ParserCombinators.ReadP

#if __GLASGOW_HASKELL__ >= 707
type Typeable1 (f :: * -> *) = Typeable f
#endif 

-- | This is just a dummy type representing a LocalBuildInfo. You don't have to use
-- this type, it is just used to tag queries and make them more type-safe.
data LocalBuildInfo = LocalBuildInfo Void deriving (Typeable, Read)

-- | A selector is a generator for a function of type i -> o.
newtype Selector i o = Selector (Version -> ExpG (i -> o))

instance Category Selector where
  id = Selector $ const id'
  Selector a . Selector b = Selector $ liftA2 (<>.) a b

-- | Compose two selectors, monadically.
(=<<<) :: Monad m => Selector b (m c) -> Selector a (m b) -> Selector a (m c)
Selector s =<<< Selector t = Selector $ \v -> applyE (flip' <>$ bind') (s v) <>. t v

-- | The same as (=<<<), but flipped.
(>>>=) :: Monad m => Selector a (m b) -> Selector b (m c) -> Selector a (m c)
(>>>=) = flip (=<<<)

-- | Lift a selector to work on functorial inputs and outputs.
fmapS :: Functor m => Selector a b -> Selector (m a) (m b)
fmapS (Selector s) = Selector $ \v -> applyE fmap' (s v)

-- | Zip together two selector that work on the same input. This is the equavilent of liftA2 (,) for selectors.
zipSelector :: Selector i o -> Selector i p -> Selector i (o,p)
zipSelector (Selector s) (Selector t) = Selector $ \v -> expr $ \i -> applyE2 tuple2 (s v <>$ i) (t v <>$ i)

-- | A Selector to get something out of a Maybe if you supply a default value.
maybeDefault :: (GenExpType a ~ a, GenExp a) => a -> Selector (Maybe a) a
maybeDefault a = selector $ const $ applyE (flip' <>$ maybe' <>$ id') $ expr a

-- | Build a selector. The expression the selector generates can depend on the cabal version.
selector :: (Version -> ExpG (i -> o)) -> Selector i o
selector = Selector

-- | A query is like a Selector, but it cannot be composed any futher using a Category instance.
-- It can have a Functor and Applicative instance though. 
-- To execute a query, you only need to run GHC once.
data Query s a = forall i. Typeable i => Query (Selector s i) (i -> a)

instance Functor (Query s) where
  fmap f (Query s x) = Query s $ f . x

instance Applicative (Query s) where
  pure = Query (selector $ const $ const' <>$ tuple0) . const
  Query f getF <*> Query a getA = Query (zipSelector f a) $ \(fv, av) -> getF fv $ getA av

-- | Build a query from a selector.
query :: Typeable a => Selector s a -> Query s a
query = flip Query id

-- | Lift a query to work over functors.
fmapQ :: (Functor f, Typeable1 f) => Query s a -> Query (f s) (f a)
fmapQ (Query s f) = Query (fmapS s) (fmap f)

-- | Use a selector to run a query in a bigger environment than it was defined in.
on :: Selector i o -> Query o r -> Query i r
on s (Query sq f) = Query (sq . s) f

getRunDirectory :: IO FilePath
getRunDirectory = getTemporaryDirectory >>= go 0
  where go :: Integer -> FilePath -> IO FilePath
        go !c dir = do 
          let cdir = dir </> "dynamic-cabal" <.> show c
          res <- E.try $ createDirectory cdir
          case res of
            Left e | isAlreadyExistsError e -> go (c + 1) dir
                   | otherwise -> E.throwIO e
            Right () -> return cdir

getCabalVersion :: FilePath -> IO Version
getCabalVersion setupConfig = do
  versionString <- dropWhile (not . flip elem ['0'..'9']) . (!! 7) . words . head . lines <$> readFile setupConfig
  case filter (null . snd) $ readP_to_S parseVersion versionString of
    [(v,_)] -> return v
    _       -> E.throwIO $ userError "Couldn't parse version"

data LeftoverTempDir e = LeftoverTempDir FilePath e deriving Typeable

instance Show e => Show (LeftoverTempDir e) where
  show (LeftoverTempDir dir e) = "Left over temporary directory not removed: " ++ dir ++ "\n" ++ show e

instance E.Exception e => E.Exception (LeftoverTempDir e)

withTempWorkingDir :: IO a -> IO a
withTempWorkingDir act = do
  pwd <- getCurrentDirectory
  tmp <- getRunDirectory
  setCurrentDirectory tmp
  res <- act `E.catch` \(E.SomeException e) -> setCurrentDirectory pwd >> E.throwIO (LeftoverTempDir tmp e)
  setCurrentDirectory pwd
  res <$ removeDirectoryRecursive tmp

generateSource :: Selector LocalBuildInfo o -> String -> FilePath -> Version -> IO (String)
generateSource (Selector s) modName setupConfig version = 
  return $ flip generateModule modName $ do
    getLBI <- addDecl (Ident "getLBI") $ 
                   applyE fmap' (read' <>. unlines' <>. applyE drop' 1 <>. lines' :: ExpG (String -> LocalBuildInfo)) 
               <>$ applyE readFile' (expr setupConfig)
    result <- addDecl (Ident "result") $ applyE fmap' (s version) <>$ expr getLBI
    return $ Just [exportFun result]

-- | Run a query. This will generate the source code for the query and then invoke GHC to run it.
runQuery :: Query LocalBuildInfo a -> FilePath -> IO a
runQuery (Query s post) setupConfig = do
  setupConfig' <- canonicalizePath setupConfig
  version <- getCabalVersion setupConfig'
  src<-  generateSource s "DynamicCabalQuery" setupConfig' version
  runRawQuery' src setupConfig post
  
-- | run a raw query, getting the full source from the first parameter
-- the module must be DynamicCabalQuery and it must have a result declaration
runRawQuery :: (Typeable a)=>String -> FilePath -> IO a
runRawQuery s setupConfig = runRawQuery' s setupConfig id
  
-- | run a raw query, getting the full source from the first parameter
-- the module must be DynamicCabalQuery and it must have a result declaration
runRawQuery' :: (Typeable i)=>String -> FilePath -> (i->a)-> IO a
runRawQuery' s setupConfig post= do
  setupConfig' <- canonicalizePath setupConfig
  withTempWorkingDir $ do
    version <- getCabalVersion setupConfig'
    writeFile "DynamicCabalQuery.hs" s
    GHC.runGhc (Just GHC.Paths.libdir) $ do
      dflags <- GHC.getSessionDynFlags
      void $ GHC.setSessionDynFlags $ dflags
             { GHC.ghcLink = GHC.LinkInMemory
             , GHC.hscTarget = GHC.HscInterpreted
             , GHC.packageFlags = [DynFlags.ExposePackage $ "Cabal-" ++ showVersion version]
             }
      dflags' <- GHC.getSessionDynFlags

      GHC.defaultCleanupHandler dflags' $ do
        target <- GHC.guessTarget "DynamicCabalQuery.hs" Nothing
        GHC.setTargets [target]
        void $ GHC.load GHC.LoadAllTargets
        GHC.setContext [GHC.IIDecl $ GHC.simpleImportDecl $ GHC.mkModuleName "DynamicCabalQuery"]
        GHC.dynCompileExpr "result" >>= maybe (fail "dynamic-cabal: runQuery: Result expression has wrong type") (MonadUtils.liftIO . fmap post) . fromDynamic
        