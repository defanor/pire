


{-# LANGUAGE FlexibleContexts #-}

-- {-# LANGUAGE FlexibleInstances #-}



{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -fno-warn-unused-matches #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


-- | Tools for working with multiple source files

-- module ReModules(getModules', ModuleInfo'(..)) where

module ReModules  where


import Syntax
import ReSyntax



import ReParse hiding (moduleImports)

-- import ReParse(parseModuleFile
--                , parseModuleFile'
--                  , parseModuleImports
--                , PiState(..), piInit)





-- import Text.ParserCombinators.Parsec.Error

import Control.Applicative 

import Control.Monad.Error


import Control.Monad.State.Lazy
import System.FilePath
import System.Directory
import qualified Data.Graph as Gr
import Data.List(nub,(\\))

import Text.PrettyPrint.ANSI.Leijen (Doc)

-- import Data.Either.Combinators(fromRight')
-- import PrettyPrint (disp)

import Modules (ModuleInfo(..))

data ModuleInfo' = ModuleInfo' {
                    modInfoName'     :: MName', 
                    modInfoFilename' :: String,
                    modInfoImports'  :: [ModuleImport']
                  }


-- runErrorT $ getModules ["", "samples/"] "Sample"

{-
  ->

 -}

getModules
  :: (Functor m
     , MonadError Doc m
     , MonadIO m) => 
     [FilePath] -> String -> m [Module]
getModules prefixes topmod = do
  toParse <- gatherModules prefixes [ModuleImport topmod]
  -- flip evalStateT piInit $ mapM reparse toParse
  evalStateT (mapM reparse toParse) piInit





-- getModules'
--   :: (Functor m, MonadError ParseError m, MonadIO m) => 
--      [FilePath] -> String -> m [Module']
-- getModules' prefixes top = do
--   toParse <- gatherModules' prefixes [ModuleImport' top]
--   flip evalStateT emptyConstructorNames $ mapM reparse' toParse




-- instance Show ModuleInfo
-- instance Show [ModuleInfo]




-- runErrorT $ gatherModules ["", "samples/"] [ModuleImport "Sample.pi"]

gatherModules
  :: (Functor m
     -- , MonadError ParseError m
     , MonadIO m
     ) =>
     [FilePath] -> [ModuleImport] -> m [ModuleInfo]
     
gatherModules prefixes ms = gatherModules_ ms [] where
  gatherModules_ [] accum = return $ topSort accum
  gatherModules_ ((ModuleImport m):ms') accum = do
    modFileName <- getModuleFileName prefixes m
    imports <- moduleImports <$> parseModuleImports modFileName
    let accum' = (ModuleInfo m modFileName imports) :accum
    let oldMods = map (ModuleImport . modInfoName) accum'
    gatherModules_ (nub (ms' ++ imports) \\ oldMods) accum'



-- gatherModules'
--   :: (Functor m, MonadError ParseError m, MonadIO m) =>
--      [FilePath] -> [ModuleImport'] -> m [ModuleInfo']
-- gatherModules' prefixes ms = gatherModules'' ms [] where
--   gatherModules'' [] accum = return $ topSort' accum
--   gatherModules'' ((ModuleImport' m):ms') accum = do
--     modFileName <- getModuleFileName' prefixes m
--     imports <- moduleImports <$> parseModuleImports modFileName
--     let accum' = (ModuleInfo' m modFileName imports) :accum
--     let oldMods = map (ModuleImport' . modInfoName) accum'
--     gatherModules'' (nub (ms' ++ imports) \\ oldMods) accum'




topSort :: [ModuleInfo] -> [ModuleInfo]
topSort ms = reverse sorted
  where (gr,lu) = Gr.graphFromEdges' 
                  [(m, modInfoName m, [i | ModuleImport i <- modInfoImports m])
                  | m <- ms]
        lu' v = let (m,_,_) = lu v in m
        sorted = [lu' v | v <- Gr.topSort gr]





-- topSort' :: [ModuleInfo'] -> [ModuleInfo']
-- topSort' ms = reverse sorted
--   where (gr,lu) = Gr.graphFromEdges' 
--                   [(m, modInfoName' m, [i | ModuleImport' _ i <- modInfoImports' m])
--                   | m <- ms]
--         lu' v = let (m,_,_) = lu v in m
--         sorted = [lu' v | v <- Gr.topSort gr]



-- instance Error ParseError
instance Error Doc


-- eg.
-- getModuleFileName ["/home/reuleaux/etc/pire/samples/"] "Sample.pi"
-- ok, seems to work

getModuleFileName :: (MonadIO m)
                  => [FilePath] -> MName -> m FilePath
getModuleFileName prefixes modul = do
  let makeFileName prefix = prefix </> mDotTrellys
      -- get M.pi from M or M.pi
      mDotTrellys = if takeExtension s == ".pi"
                    then s
                    else s <.> "pi"
      s = modul
      possibleFiles = map makeFileName prefixes
  files <- liftIO $ filterM doesFileExist possibleFiles
  if null files
     then error $ "Can't locate module: " ++ show modul ++
                "\nTried: " ++ show possibleFiles
     else return $ head files



getModuleFileName' :: (MonadIO m)
                  => [FilePath] -> MName' -> m FilePath
getModuleFileName' prefixes (MName' modul  _) = do
  let makeFileName prefix = prefix </> mDotTrellys
      mDotTrellys = if takeExtension s == ".pi"
                    then s
                    else s <.> "pi"
      s = modul
      possibleFiles = map makeFileName prefixes
  files <- liftIO $ filterM doesFileExist possibleFiles
  if null files
     then error $ "Can't locate module: " ++ show modul ++
                "\nTried: " ++ show possibleFiles
     else return $ head files



-- orig from Modules.hs
{-
  -- | Fully parse a module (not just the imports).
  reparse :: (MonadError ParseError m, MonadIO m, MonadState ConstructorNames m) => 
              ModuleInfo -> m Module
  reparse (ModuleInfo _ fileName _) = do
    cnames <- get
    modu <- parseModuleFile cnames fileName
    put (moduleConstructors modu)
    return modu
 -}



reparse :: (
  -- MonadError ParseError m
  MonadError Doc m
  , MonadIO m
  , MonadState PiState m) => 
           ModuleInfo -> m Module
reparse (ModuleInfo _ fn _) = do
  st <- get
  modu <- parseModuleFile st fn
  put (st { constr_names = moduleConstructors modu})
  return modu





reparse' :: (
            MonadIO m
            , MonadState PiState m
            ) => 
            ModuleInfo' -> m Module'

reparse' (ModuleInfo' _ fn _) = do
  st <- get

  modu <- parseModuleFile' st fn
  put (st { constr_names = moduleConstructors' modu})
  return modu



