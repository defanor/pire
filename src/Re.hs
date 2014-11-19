


{-

disp $ (subst (s2n "fac") (Var $ s2n "ffff")) $ fromSuccess $ parseExpr'  "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "

disp $ fromSuccess $ parseExpr'  "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "

disp $ fromRight' $ parseExpr  "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "

-}


{-# LANGUAGE MultiParamTypeClasses #-}





{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}


-- rx
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}


{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- {-# OPTIONS_GHC -fwarn-unused-matches #-}
-- {-# OPTIONS_GHC -fwarn-unused-binds #-}



-- | The command line interface to the pi type checker. 
-- Also provides functions for type checking individual terms
-- and files.


module Re(goFilename,go,main, test) where


-- import Modules (getModules)
-- -- import ReModules (getModules)


import qualified Modules as Mod
import qualified ReModules as ReMod


import PrettyPrint
import Environment
import TypeCheck

import Parser
-- import ReParse



import Text.PrettyPrint.HughesPJ (render)
import Text.ParserCombinators.Parsec.Error 

-- deprecated
import Control.Monad.Error

-- import Control.Monad.Except

-- import Control.Monad.Trans.Except (runExceptT)
-- but needed so far...
-- import Control.Monad.Trans.Error (runErrorT)


-- import Control.Monad (forM_)


import System.Environment(getArgs)
import System.Exit (exitFailure,exitSuccess)
import System.FilePath (splitFileName)


import Unbound.LocallyNameless


-- cabal install either
import Data.Either.Combinators (fromRight')


-- libghc-pretty-show-dev
-- http://hackage.haskell.org/package/pretty-show
-- dann P.ppShow
-- bzw putStrLn $ P.ppShow foo
-- import qualified Text.Show.Pretty as P


-- für die Module accessors
import Syntax



exitWith :: Either a b -> (a -> IO ()) -> IO b
exitWith res f = 
  case res of 
    Left x -> f x >> exitFailure 
    Right y -> return y
    
-- | Type check the given string in the empty environment
go :: String -> IO ()
go s = do
  case parseExpr s of
    Left parseError -> putParseError parseError
    Right term'' -> do 
      putStrLn "parsed as"
      putStrLn $ render $ disp term''
      res <- runTcMonad emptyEnv (inferType term'')
      case res of 
        Left typeError -> putTypeError typeError
        Right (aterm, ty) -> do
          putStrLn $ render $ disp aterm
          putStrLn "typed with type"
          putStrLn $ render $ disp ty
  
-- | Display a parse error to the user  
putParseError :: ParseError -> IO ()  
putParseError parseError = do
  putStrLn $ render $ disp $ errorPos parseError
  putStrLn $ show parseError
  
-- | Display a type error to the user  
putTypeError :: Disp d => d -> IO ()  
putTypeError typeError = do 
  putStrLn "Type Error:"
  putStrLn $ render $ disp typeError


-- | Type check the given file    
goFilename :: String -> IO ()  
goFilename pathToMainFile = do
  let prefixes = currentDir : mainFilePrefix : []
      (mainFilePrefix, name') = splitFileName pathToMainFile
      currentDir = "" 
  putStrLn $ "processing " ++ name' ++ "..."
  -- v <- runErrorT (getModules prefixes name')
  v <- runExceptT (Mod.getModules prefixes name')
  val <- v `exitWith` putParseError
  putStrLn "type checking..."
  d <- runTcMonad emptyEnv (tcModules val)
  defs <- d `exitWith` putTypeError
  putStrLn $ render $ disp (last defs)


      
-- | Type check the given file    
goFilename' :: String -> IO ()  
goFilename' pathToMainFile = do
  let prefixes = currentDir : mainFilePrefix : []
      (mainFilePrefix, name') = splitFileName pathToMainFile
      currentDir = "" 

  putStrLn $ "processing " ++ name' ++ "..."
  -- v <- runErrorT (getModules prefixes name')
  v <- runExceptT (ReMod.getModules prefixes name')
  val <- v `exitWith` putParseError

  
  putStrLn "type checking..."
  d <- runTcMonad emptyEnv (tcModules val)
  defs <- d `exitWith` putTypeError

          
  -- putStrLn $ render $ disp (last defs)
  -- putStrLn $ render $ val

  return ()



-- mods "mystuff/Nat.pi" >>= return . disp . head
-- mods_ "mystuff/Nat.pi" >>= return . disp . head



-- runExceptT (Mod.getModules ["", "mystuff/"] "Nat2")
-- runExceptT (ReMod.getModules ["", "mystuff/"] "Nat2")
-- runExceptT (Mod.getModules ["", "mystuff/"] "Nat2") >>= return . head . fromRight'


-- runExceptT (parseModuleFile emptyConstructorNames "mystuff/Nat2.pi")
-- runExceptT (parseModuleFile emptyConstructorNames "mystuff/Nat2.pi") >>= return . fromRight'

-- vs ReParse
-- parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef) piInit) "mystuff/Nat2.pi" >>= return . fromSuccess



mods pathToMainFile = do
  let prefixes = currentDir : mainFilePrefix : []
      (mainFilePrefix, name') = splitFileName pathToMainFile
      currentDir = "" 

  putStrLn $ "processing " ++ name' ++ "..."
  -- v <- runErrorT (getModules prefixes name')
  v <- runExceptT (Mod.getModules prefixes name')
  val <- v `exitWith` putParseError
  return val


mods_ pathToMainFile = do
  let prefixes = currentDir : mainFilePrefix : []
      (mainFilePrefix, name') = splitFileName pathToMainFile
      currentDir = "" 

  putStrLn $ "processing " ++ name' ++ "..."
  -- v <- runErrorT (getModules prefixes name')
  v <- runExceptT (ReMod.getModules prefixes name')
  val <- v `exitWith` putParseError
  return val



defs' pathToMainFile = do
  ms <- mods pathToMainFile
  putStrLn "type checking..."
  d <- runTcMonad emptyEnv (tcModules ms)
  ds <- d `exitWith` putTypeError
  return ds


-- Term
-- Var $ s2n "hallo"

-- :t subst $ s2n "hallo"

-- subst (s2n "blll") (Var $ s2n "aber") (Var $ s2n "blll")

-- disp $ fromRight' $ parseExpr "\\x .a "

-- disp $ fromRight' $ parseExpr " fac = \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))"

-- disp $ subst (s2n "fac") (Var $ s2n "foo") $ fromRight' $ parseExpr " fac = \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))"


-- let fac = fromRight' $ parseExpr " fac = \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))"
-- disp fac
-- disp $ subst (s2n "fac") (Var $ s2n "FAC") fac


-- let bar = fromRight' $ parseExpr " bar = \\n. let fac = ((\\x . plus x 2) : Nat -> Nat) in plus (foo n) (fac (mult 3 n)) "
-- disp bar
-- disp $ subst (s2n "fac") (Var $ s2n "FAC") bar



  

-- class Rename a where
--   rename :: a -> TName -> TName -> a

-- instance Rename Decl where
--   rename sig@(Sig tname term) from' to'
--     | tname == from'  = Sig to' term
--     | otherwise = sig



-- subst terms for vars in decls
-- wording cf unbound-docs/papers+exmpls/unbound-0.4.3.1/tutorial/Tutorial.html
instance Subst Term Decl where
-- instance Subst Decl Decl where


  --  isvar = const Nothing 
  -- isvar (RecDef a (Var x)) = Just (SubstName x)
  -- expected Term -> SubstName Decl Term
  -- isvar (RecDef a t ) = Just (SubstName (RecDef a) t)
  


-- rename r@(RecDef tname bla) (Subst x y) =
 

rename r@(RecDef tname bla') =
  RecDef tname' bla''
  where
    tname' = s2n "fffff" 
    bla'' = subst (s2n "fac") (Var $ s2n "fffff") bla'


rename r@(Def tname bla') =
  RecDef tname' bla''
  where
    tname' = s2n "fffff" 
    bla'' = subst (s2n "fac") (Var $ s2n "fffff") bla'
rename r@(_) = r


renameModule :: Module -> Module
renameModule m@(
  Module { moduleName=n,
           moduleImports=is,
           moduleEntries=decls
         , moduleConstructors=constrs
         }) =
  
  Module {moduleName=n ,
          moduleImports=is,
          moduleEntries = map (\d -> subst (s2n "fac") (Var $ s2n "fffff") d) decls,
          -- moduleEntries = map (\d -> rename d) decls,
          moduleConstructors=constrs
          }

-- P.ppShow `liftM` defs' "mystuff/Fac.pi" >>= putStrLn
-- P.ppShow `liftM` defs' "mystuff/Fac.pi" >>= return
-- P.ppShow `liftM` defs' "mystuff/Fac.pi" >>= putStrLn
-- (P.ppShow . last) `liftM` defs' "mystuff/Fac.pi" >>= return
-- P.ppShow `liftM` defs' "mystuff/Fac.pi" >>= return . putStrLn


-- last `liftM` defs' "mystuff/Fac.pi" >>= return . disp
-- (disp . last) `liftM` defs' "mystuff/Fac.pi"
-- (disp . renameModule . last) `liftM` defs' "mystuff/Fac.pi"



bla pathToMainFile = do
  ds <- defs' pathToMainFile

  
  putStrLn ""
  

-- s <- (show . fromRight') `liftM` runErrorT (getModules prefixes name)
  -- mods <- fromRight' `liftM` runErrorT (getModules prefixes name)

  -- putStrLn $ P.ppShow $ ds
  -- putStrLn ""


  -- forM_ ds (\m ->

  --              -- putStrLn $ P.ppShow $ moduleEntries m

  --              forM_ (moduleEntries m) (\decl ->
  --                             putStrLn $ P.ppShow $ decl
  --                                      ) 

  --              )


  -- putStrLn ""

  let last' = last ds
  
  
  forM_(moduleEntries $ last') (\decl' ->
                                   putStrLn $ (P.ppShow $ decl')++"\n"
                                   
                                 ) 
    
    

  putStrLn $ render $ disp (last')
  putStrLn ""

  putStrLn $ render $ disp (renameModule last')


  return ()







-- fromRight' $ parseExpr "x"
-- fromRight' $ parseExpr "(x:A)"
-- fromRight' $ parseExpr "\\  x . a"


-- let pathToMainFile = "mystuff/Nat.pi"
-- let (mainFilePrefix, name) = splitFileName pathToMainFile
-- let currentDir = ""
-- let prefixes = currentDir : mainFilePrefix : []
-- fromRight' $ runErrorT (getModules prefixes name)
-- fromRight' `liftM` runErrorT (getModules prefixes name)




-- the parser exports
--    parseModuleFile, 
--    parseModuleImports,
--    parseExpr




test :: IO ()
test = do
  goFilename "../test/Lec1.pi"
  goFilename "../test/Hw1.pi"  
  goFilename "../test/Lec2.pi"
  goFilename "../test/Hw2.pi"  
  goFilename "../test/Lec3.pi"
  goFilename "../test/Fin1.pi"
  goFilename "../test/Lec4.pi"
      
  goFilename "../test/Logic.pi"  
  goFilename "../test/Equality.pi"  
  goFilename "../test/Product.pi"  
  goFilename "../test/Nat.pi"  
  goFilename "../test/Fin.pi"  
  goFilename "../test/Vec.pi"  
        
  

-- | 'pi <filename>' invokes the type checker on the given 
-- file and either prints the types of all definitions in the module
-- or prints an error message.
main :: IO ()
main = do
  [pathToMainFile] <- getArgs
  goFilename pathToMainFile
  exitSuccess
  
