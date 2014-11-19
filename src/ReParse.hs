



{-# OPTIONS_GHC -fno-warn-orphans #-}


-- for deriving Functor
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- {-# LANGUAGE DeriveFunctor #-}
-- {-# LANGUAGE StandaloneDeriving #-}


-- for impOrExpVar
{-# LANGUAGE TupleSections #-}


-- instances...
-- {-# LANGUAGE OverlappingInstances #-}



-- in idris > ParseHelpers.hs via AbsSyntax.hs (?)
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE FlexibleInstances #-}


-- for TokParsing, MonadicParsing
{-# LANGUAGE ConstraintKinds #-}


-- MonadError
{-# LANGUAGE FlexibleContexts #-}



-- {-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ExplicitForAll #-}


-- for MarkParsing of FreshMT...
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE UndecidableInstances #-}


-- for
-- instance MonadState PiState InnerParser' where
{-# LANGUAGE MultiParamTypeClasses #-}


-- natenc'
{-# LANGUAGE LambdaCase #-}


{-

Concrete syntax for the language:
Optional components in this BNF are marked with < >

...
-}





module ReParse where

-- module Parser


--   (
--    parseModuleFile,
--    parseModuleImports,
--    parseExpr
--   )
--   where


-- import ReToken

import ReSyntax




-- cabal install either
-- import Data.Either.Combinators
-- then: fromRight'

-- analog fromJust
import Data.Maybe (fromJust, isJust, isNothing)



-- only for disp, testing
import PrettyPrint (disp)
import RePP



import Control.Applicative

-- MonadPlus
import Control.Monad

import Control.Lens
import Data.Char (isSpace)

import Text.Trifecta

import Text.Trifecta.Delta



import Text.Parser.Token.Style

import Text.Parser.Token.Highlight as Hi
-- import Text.Parser.Token.Highlight


import Text.Parser.Expression(Operator(..),Assoc(..),buildExpressionParser)


import Text.Parser.LookAhead


-- foldl'
import Data.List (foldl')


import qualified Data.HashSet as HS


import Syntax hiding (moduleImports)

-- string2Name
import Unbound.LocallyNameless hiding (Data,Refl,Infix,join,name)

import Unbound.LocallyNameless.Fresh


import Control.Monad.Trans

-- throwError
-- deprecated
import Control.Monad.Error hiding (join)
-- import Control.Monad.Except hiding (join)


import Text.PrettyPrint.ANSI.Leijen (Doc)

-- import Control.Monad.State


import Control.Monad.State.Strict


import qualified Data.Set as S

-- source pos
import Text.ParserCombinators.Parsec.Pos



import Data.Text.Encoding (decodeUtf8)
-- import qualified Data.ByteString.Char8 as BS
-- import qualified Data.ByteString.Lazy.Char8 as LBS
import Data.Text (unpack)

import qualified Data.ByteString.UTF8 as UTF8



-- simple pretty printer
-- use w/ P.ppShow foo
-- resp putStrLn $ Pr.ppShow foo
-- import qualified Text.Show.Pretty as P




newtype InnerParser a = InnerParser { runInnerParser :: Parser a }
                      deriving (Functor
                               , Monad
                               , Applicative
                               , Alternative
                               , Parsing
                               , CharParsing
                               , MonadPlus
                               -- , TokenParsing
                               , DeltaParsing
                               , LookAheadParsing
                               )

instance TokenParsing InnerParser where
  someSpace = buildSomeSpaceParser (skipSome (satisfy isSpace))
              $ commentStart .~ "{-"
              $ commentEnd .~ "-}"
              $ commentLine .~ "--"
              $ commentNesting .~ True
              $ emptyCommentStyle








-- instance (CharParsing m, MonadPlus m) => TokenParsing (FreshMT m) where

instance (TokenParsing m, MonadPlus m) => TokenParsing (FreshMT m) where
  someSpace = lift someSpace

-- instance (CharParsing m, MonadPlus m) => TokenParsing (FreshMT m) where
--   someSpace = buildSomeSpaceParser (skipSome (satisfy isSpace))
--               $ commentStart .~ "{-"
--               $ commentEnd .~ "-}"
--               $ commentLine .~ "--"
--               $ commentNesting .~ True
--               $ emptyCommentStyle




instance (Alternative m, MonadPlus m) => Alternative (FreshMT m) where
  (FreshMT m) <|> (FreshMT l) = FreshMT (m <|> l)
  -- empty = FreshMT Control.Applicative.empty
  empty = FreshMT mzero


  -- some (FreshMT m) = FreshMT $ some m
  -- many (FreshMT m) = FreshMT $ many m


instance (Parsing m, MonadPlus m) => Parsing (FreshMT m) where
  try (FreshMT m) = FreshMT $ try m
  FreshMT m <?> l = FreshMT (m <?> l)
  notFollowedBy (FreshMT m) = FreshMT (notFollowedBy m)


instance (LookAheadParsing m, MonadPlus m) => LookAheadParsing (FreshMT m) where
  lookAhead (FreshMT m) = FreshMT $ lookAhead m



instance (CharParsing m, MonadPlus m) => CharParsing (FreshMT m) where
  -- string = lift . string
  -- char    = lift . char
  -- notChar = lift . notChar
  -- anyChar = lift anyChar
  -- text = lift . text



instance (DeltaParsing m, MonadPlus m) => DeltaParsing (FreshMT m) where
  line = lift line
  position = lift position
  slicedWith f (FreshMT m) = FreshMT $ slicedWith f m


-- instance MarkParsing d m => MarkParsing d (FreshMT m) where
--   mark = lift mark
--   release = lift . release





instance LookAheadParsing m => LookAheadParsing (Unspaced m) where
  lookAhead (Unspaced m) = Unspaced (lookAhead m)



instance (MonadPlus m, DeltaParsing m) => DeltaParsing (Unspaced m) where
  position = lift position
  line = lift line
  slicedWith f (Unspaced m) = Unspaced $ slicedWith f m


instance (Fresh m) => Fresh (Unspaced m) where
  fresh = lift . fresh

instance (MonadState PiState m) => MonadState PiState (Unspaced m) where




data PiState = PiState {
  indent_stack :: [Int]
  , brace_stack :: [Maybe Int]
  , constr_names :: ConstructorNames

  } deriving (Show)




piInit :: PiState


piInit = PiState [] [] emptyConstructorNames






{-
  adjusted from emptyConstructorNames

  recall

  data ConstructorNames = ConstructorNames {
                            tconNames :: Set String,
                            dconNames :: Set String
                          }
    deriving (Show, Eq)

  emptyConstructorNames :: ConstructorNames
  emptyConstructorNames = ConstructorNames S.empty S.empty
 -}



myPrelude :: ConstructorNames
myPrelude = ConstructorNames
            (S.fromList ["Nat"])
            (S.fromList ["Zero", "Succ"])


piPrelude = PiState [] [] myPrelude


-- as I understand it (?):

-- just within one line
-- top = Columns 0 0

-- line range, col range
-- but:
-- column $ Lines 3 17 24 100
-- 17
-- therefore maybe: top left, bottom right (?)
top = Lines 0 0 0 0




-- type PiParser = FreshT Integer

-- -- type PiParser = FreshT InnerParser

-- type PiParser = FreshMT InnerParser

-- type PiParser = StateT PiState InnerParser


type PiParser = StateT PiState (FreshMT InnerParser)


-- instance TokenParsing (StateT PiState (FreshMT InnerParser)) where
--   someSpace = lift  someSpace

--   -- need those as well
--   -- token (F m) = F $ token m







-- generalized monadic parsing constraint type
type MonadicParsing m =
  (DeltaParsing m, LookAheadParsing m, TokenParsing m, Monad m)


type TokParsing m = (TokenParsing m, Monad m)




idStyle :: TokenParsing m => IdentifierStyle m
idStyle = styleStart .~ letter
          $ styleLetter .~ (alphaNum <|> oneOf "_'")
          $ styleReserved .~  HS.fromList
          ["refl"
          ,"ind"
          ,"Type"
          ,"data"
          ,"where"
          ,"case"
          ,"of"
          ,"with"
          ,"contra"
          ,"subst", "by", "at"
          ,"let", "in"
          ,"axiom"
          ,"erased"
          ,"TRUSTME"
          ,"ord"
          , "pcase"
          , "Bool", "True", "False"
          ,"if","then","else"
          , "One", "tt"
          ]
          $ emptyIdents





liftError :: MonadError Doc m => Result a  -> m a
liftError (Failure d) = throwError d
liftError (Success  a) = return a



-- parseDecl' = parseString (runInnerParser $ runFreshMT $ whiteSpace *> decl) (Columns 0 0)




-- cf. Ed Kmett's either / fromRight'
fromSuccess :: Result b -> b
fromSuccess (Success x) = x
fromSuccess (Failure doc') = error $ show doc'


-- ====================
-- some helpers


-- cf
-- fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef  ) piInit) "mystuff/Nat.pi" >>= return . disp
-- parseModuleFile'' piInit "mystuff/Nat.pi" >>= return . disp



-- orig from Parser.hs
-- | Parse a module declaration from the given filepath.
-- parseModuleFile :: (MonadError ParseError m, MonadIO m) => ConstructorNames -> String -> m Module
-- parseModuleFile cnames name = do
--   liftIO $ putStrLn $ "Parsing File " ++ show name
--   contents <- liftIO $ readFile name
--   liftError $ runFreshM $
--     flip evalStateT cnames $
--      (runParserT (do { whiteSpace; v <- moduleDef;eof; return v}) [] name contents)


-- parseModuleFile :: (MonadIO m) => PiState -> String -> m Module
parseModuleFile :: (MonadIO m, MonadError Doc m) => PiState -> FilePath -> m Module
parseModuleFile st name = do
  liftIO $ putStrLn $ "Parsing File " ++ show name

  -- later
  --  ...(whiteSpace *> moduleDef <* eof)...
  -- wo/ eof


  -- fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef) st) name
  -- better:
  contents <- liftIO $ readFile name
  liftError $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef) st ) top contents



parseModuleFile' :: (MonadIO m) => PiState -> String -> m Module'
parseModuleFile' st name = do
  liftIO $ putStrLn $ "Parsing File " ++ show name
  fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef') st) name






-- parseModuleFile'' piInit "mystuff/Nat.pi" >>= return . disp

parseModuleFile_ :: MonadIO m => PiState -> String -> m Module
parseModuleFile_ st name = do
  liftIO $ putStrLn $ "Parsing File " ++ show name
  contents <- liftIO $ readFile name

  -- liftError $ runFreshM $
  --   flip evalStateT cnames $
  -- (runInnerParserT (do { whiteSpace; v <- moduleDef;eof; return v}) [] name contents)


  return $ fromSuccess $ parseString
    (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef) st )
    (Columns 0 0)
    contents



-- parseModuleFile__ piInit "module bla where"

-- parseModuleFile__ st =
--   parseString
--   (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef <* eof) st)
--   (Columns 0 0)



parseModuleImports name = do
  parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleImports) piInit) name
  >>= return . fromSuccess


parseExpr' = parseString
             (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit)
             (Columns 0 0)



-- ====================
-- the real stuff - parsing


str = unpack . decodeUtf8



identifier :: (TokenParsing m, Monad m) => m String
identifier = token $ ident $ idStyle




id' :: (TokenParsing m
       , Monad m
       , DeltaParsing m
       )
       => m (String, Ws)
id' = do
  i <- runUnspaced identifier
  ws <- sliced whiteSpace
  return $ (i, Ws $ str ws)




variable :: (TokenParsing m
            , Monad m
            ) => m TName
variable =
  do i <- identifier
     return $ s2n i






var' :: (TokenParsing m
         , Monad m
         , DeltaParsing m
         )
        => m (TName', Ws)
var' = do
  i <- runUnspaced identifier
  ws <- sliced whiteSpace
  return $ (s2n i, Ws $ str ws)







reserved :: (TokenParsing m, Monad m) => String -> m ()
reserved = token . reserve idStyle




res' :: (TokenParsing m
        , Monad m
        , DeltaParsing m
        )
             => String -> m Ws
res' s = do
  runUnspaced $ reserved s
  ws <- sliced whiteSpace
  return $ Ws $ str ws


-- hm, does it work ? or what does it do ex' ?
-- still possible eg. reservedOp "hi"

reservedOp :: TokParsing m => String -> m ()
reservedOp =
  token . reserve (styleStart .~ sym $ styleLetter .~ sym $ emptyOps)

  where sym = oneOf $ Prelude.concat
              ["!","?","\\",":",".",",","<", "=", "+", "-", "^", "()", "_","|","{", "}"]


resOp' :: (TokParsing m,
           DeltaParsing m)
               => String -> m Ws
resOp' s = do
  runUnspaced $ reservedOp s
  ws <- sliced whiteSpace
  return $ Ws $ str ws





-- eg. foo, [foo], [ bar  ]

impOrExpVar :: (TokParsing m, DeltaParsing m, MonadState PiState m)
               => m (TName, Epsilon)
impOrExpVar = try ((,Erased) <$> (brackets variable))
              <|> (,Runtime) <$> variable



-- hm, difficult, beware of
-- "  [{-before-}foo{-after-}]{-and so on-}  "
-- vs just
-- "  [foo]{-and so on-}  "
-- there redone above



bracketOpen' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m Token
bracketOpen' = do
  runUnspaced $ symbol "["
  ws <- sliced whiteSpace
  return $ BracketOpen $ Ws $ str ws


bracketClose' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m Token
bracketClose' = do
  runUnspaced $ symbol "]"
  ws <- sliced whiteSpace
  return $ BracketClose $ Ws $ str ws




-- return only the TName' (?)

impOrExpVar' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m (TName', Epsilon)
impOrExpVar' =
  try ((,Erased) <$> (do
                         _ <- bracketOpen'
                         (v, _)  <- var'
                         _ <- bracketClose'
                         return v
                     ))
  <|> (,Runtime) <$> (var' >>= \(v, _) -> return v)




-- return Term', not just TName' !
-- really don't need the Epsilon, I guess

impOrExpVar'' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m (Term', Epsilon)
impOrExpVar'' =
  try (do
          bo <- bracketOpen'
          (n, ws)  <- var'
          bc <- bracketClose'
          return (BtVar' bo n ws bc, Erased)
       )
  <|> (var' >>= \(n, ws) -> return (Var' n ws, Runtime))







wildcard :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m TName
wildcard = reservedOp "_" >> return wildcardName


-- todo rethink: _ requires a space afterwards

wildcard' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
             => m (TName', Ws)
wildcard' = resOp' "_" >>= \ws -> return (wildcardName', ws)



varOrWildcard :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m TName
varOrWildcard = try wildcard <|> variable



-- varOrWildcard' :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m TName'
-- varOrWildcard' = try (wildcard' >>= \(tn, _) -> return tn)  <|> (var' >>= \(v, _) -> return v)



varOrWildcard' :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m (TName', Ws)
varOrWildcard' = try wildcard'  <|> var'







-- ====================
-- terms or exprs




-- variables or zero-argument constructors


varOrCon :: (TokenParsing m
            , MonadState PiState m
            -- , DeltaParsing m
            ) => m Term
varOrCon = do i <- identifier

              -- cnames <- get
              st <- get
              let cnames = constr_names st

              if  (i `S.member` (dconNames cnames))
                then return (DCon i [] (Annot Nothing))
                else if  (i `S.member` tconNames cnames)
                       then return (TCon i [])
                       else return (Var (string2Name i))



varOrCon' :: (TokenParsing m, MonadState PiState m, DeltaParsing m) => m Term'
varOrCon' = do
  (i, ws) <- id'

  st <- get
  let cnames = constr_names st

  if  (i `S.member` (dconNames cnames))
    then
    return (DCon' (DCName' i ws) [] (Annot' Nothing (Ws "")))
    else if  (i `S.member` tconNames cnames)
         then
           return (TCon' (TCName' i ws) [])
         else
           return $ Var' (s2n i) ws






-- ********************






trustme :: TokParsing m => m  Term
trustme = do reserved "TRUSTME"
             return (TrustMe (Annot Nothing))

trustme' :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m Term'
trustme' = do
  ws <- res' "TRUSTME"
  return $ TrustMe' $ Annot' Nothing ws



refl :: (TokenParsing m
        , Monad m) => m  Term
refl =
  do reserved "refl"
     return $ Refl (Annot Nothing)


refl' :: (TokenParsing m
        , Monad m
        , DeltaParsing m
        ) => m  Term'
refl' = do
  ws <- res' "refl"
  return $ Refl' (Annot' Nothing $ Ws "")





typen :: TokParsing m => m Term
typen =
  do reserved "Type"
     return Type


typen' :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m Term'
typen' = do
  ws <- res' "Type"
  return $ Type' ws




bconst  :: (TokParsing m
           -- , MonadState PiState m
           -- , DeltaParsing m
           ) => m Term
bconst = choice [reserved "Bool"  >> return TyBool,
                 reserved "False" >> return (LitBool False),
                 reserved "True"  >> return (LitBool True),
                 reserved "One"   >> return TyUnit,
                 reserved "tt"    >> return LitUnit]


bconst' :: (TokParsing m, DeltaParsing m, MonadState PiState m) => m Term'
bconst' = choice $ tail [
  undefined

  , res' "Bool"  >>= \ws -> return $ TyBool' ws
  , res' "False" >>= \ws -> return (LitBool' False ws)
  , res' "True"  >>= \ws -> return (LitBool' True ws)
  , res' "One"   >>= \ws -> return $ TyUnit' ws
  , res' "tt"    >>= \ws -> return $ LitUnit' ws
  ]




ifExpr :: (TokenParsing m
          , Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m) => m  Term
ifExpr =
  do reserved "if"
     a <- expr
     reserved "then"
     b <- expr
     reserved "else"
     c <- expr
     return (If a b c (Annot Nothing))

     {-
     let tm = Match (bind (PatCon "True"  []) b)
     let fm = Match (bind (PatCon "False" []) c)
     return $ (Case a [tm, fm] (Annot Nothing))
     -}






ifExpr' :: (TokenParsing m, Fresh m, LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m) => m  Term'
ifExpr' =
  do
    i <- res' "if"
    a <- expr'
    t <- res' "then"
    -- t <- then'
    b <- expr'
    e <- res' "else"
    c <- expr'
    return (If' i a t b e c (Annot' Nothing $ Ws ""))


     {-
     let tm = Match (bind (PatCon "True"  []) b)
     let fm = Match (bind (PatCon "False" []) c)
     return $ (Case a [tm, fm] (Annot Nothing))
     -}



-- Lambda abstractions have the syntax '\x . e'
lambda :: (TokenParsing m
          , Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m
          ) => m Term
lambda = do reservedOp "\\"
            binds <- some   impOrExpVar
            dot
            body <- expr
            return $ foldr lam body binds
  where
    lam (x, Runtime) m = Lam (bind (x, embed $ Annot Nothing) m)

    lam (x, Erased) m  = ErasedLam (bind (x, embed $ Annot Nothing) m)





-- todo still missing: ws after dot

lambda' :: (TokenParsing m
           , Fresh m
           , LookAheadParsing m
           , DeltaParsing m
           , MonadState PiState m
           ) => m Term'
lambda' = do ws <- resOp' "\\"
             binds <- some impOrExpVar''
             dot
             body <- expr'
             let l = foldr lam body binds
             return $ (\case Lam' _ b             -> Lam' ws b
                             ErasedLam' _ bo b bc -> ErasedLam' ws bo b bc) l
  where
    lam (Var' n ws, Runtime) m
      = Lam' (Ws "") (bind (n, embed $ Annot' Nothing ws) m)

    lam (BtVar' bo n ws bc, Erased) m
      = ErasedLam' (Ws "") bo (bind (n, embed $ Annot' Nothing ws) m) bc



letExpr :: (TokParsing m
           , LookAheadParsing m
           , DeltaParsing m
           , MonadState PiState m
           , Fresh m
            ) => m Term
letExpr = do
  reserved "let"
  x <- variable
  reservedOp "="
  boundExp <- expr
  reserved "in"
  body <- expr
  return $ (Let (bind (x,embed boundExp) body))




-- ws after "let", name, "=", and "in"
letExpr' :: (TokParsing m
           , LookAheadParsing m
           , DeltaParsing m
           , MonadState PiState m
           , Fresh m
            ) => m Term'
letExpr' = do
  ws <- res' "let"
  (x, ws') <- var'
  ws'' <- res' "="
  boundExp <- expr'
  ws''' <- res' "in"
  body <- expr'
  return $ (Let' ws ws' ws'' ws''' (bind (x,embed boundExp) body))




factor :: (TokenParsing m
          , Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m
          ) => m Term
factor = choice [

  varOrCon   <?> "a variable or nullary data constructor"

  , typen      <?> "Type"

  , lambda     <?> "a lambda"

  , letExpr    <?> "a let"

  , natenc     <?> "a literal"

  , caseExpr   <?> "a case"

  , substExpr  <?> "a subst"

  , refl       <?> "refl"

  , trustme    <?> "TRUSTME"

  , bconst     <?> "a (boolean) constant"

  , ifExpr     <?> "an if expression"

  -- , sigmaTy    <?> "a sigma type"
  --   -- , pcaseExpr  <?> "a pcase"

  , expProdOrAnnotOrParens
    <?> "an explicit function type or annotated expression"
  ]




factor' :: (TokenParsing m, Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m) => m Term'
factor' = choice [


   varOrCon'   <?> "a variable or nullary data constructor"


  , typen'      <?> "Type"


  , lambda'     <?> "a lambda"

  , letExpr'    <?> "a let"


  , natenc'     <?> "a literal"

  , caseExpr'   <?> "a case"

  , substExpr'  <?> "a subst"

  , refl'       <?> "refl"

  , trustme'    <?> "TRUSTME"

  , bconst'     <?> "a (boolean) constant"

  , ifExpr'     <?> "an if expression"

  -- , sigmaTy    <?> "a sigma type"
  --   -- , pcaseExpr  <?> "a pcase"

  , expProdOrAnnotOrParens'
    <?> "an explicit function type or annotated expression"
  ]



arg :: (TokenParsing m
       , LookAheadParsing m
       , DeltaParsing m
       , MonadState PiState m
       , Fresh m
       ) => m Arg
arg = (Arg Erased) <$> brackets expr <|> (Arg Runtime) <$> factor


dconapp :: (TokenParsing m
           ,DeltaParsing m
           , MonadState PiState m
           , LookAheadParsing m
           , Fresh m
           ) => m Term

dconapp = do
  c <- dconstructor
  -- args <- many arg
  args <- many (do notEndApp; arg)
  return $ DCon c args (Annot Nothing)



-- tconapp :: (TokenParsing m
--            ,DeltaParsing m
--            , MonadState PiState m
--            , LookAheadParsing m
--            , Fresh m
--            ) => m Term
-- tconapp = do
--   c <- tconstructor
--   ts <- many factor
--   return $ TCon c ts





funapp :: (
  TokenParsing m
  , Fresh m
  , LookAheadParsing m
  , DeltaParsing m
  , MonadState PiState m
  ) => m Term
funapp = do
  f <- factor


  -- notEndApp; stolen from idris ParseExpr

  foldl' app f <$> many (do notEndApp; bfactor)

  where
        bfactor = factor
        app = App




funapp' :: (TokenParsing m, Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m) => m Term'
funapp' = do
  f <- factor'

  -- notEndApp: stolen from idr ParseExpr

  foldl' app f <$> many (do notEndApp; bfactor)
    where
        bfactor = factor'
        app = App'





-- A "term" is either a function application or a constructor
-- application.  Breaking it out as a seperate category both
-- eliminates left-recursion in (<expr> := <expr> <expr>) and
-- allows us to keep constructors fully applied in the abstract syntax.


term :: (TokenParsing m
        , Fresh m
        , LookAheadParsing m
        , DeltaParsing m
        , MonadState PiState m
        ) => m Term
-- term =  funapp
-- term = try dconapp <|>  try tconapp <|>  funapp
term = try dconapp <|>  funapp



term' :: (TokenParsing m, Fresh m, LookAheadParsing m
        , DeltaParsing m
        ,MonadState PiState m) => m Term'
term' =  funapp'





-- Pos from Syntax
-- import Text.ParserCombinators.Parsec.Pos


-- expr is the toplevel expression grammar


expr :: (TokenParsing m
        , Fresh m
        , LookAheadParsing m
        , DeltaParsing m
        , MonadState PiState m
        ) => m Term
expr = do
  pos <- position

  -- let p = newPos "" (lineNum pos) (columnNum pos)
  let p = newPos (fileName pos) (lineNum pos) (columnNum pos)


  Pos p <$> (buildExpressionParser table term)

  -- buildExpressionParser table term

  where table =
          [
            [ifix  AssocLeft "=" TyEq]
          , [ifixM AssocRight "->" mkArrow]
          ]

        ifix  assoc op' f = Infix (reservedOp op' >> return f) assoc
        ifixM assoc op' f = Infix (reservedOp op' >> f) assoc
        mkArrow  =
          do n <- fresh wildcardName
             return $ \tyA tyB ->
               Pi (bind (n,embed tyA) tyB)





expr' :: (TokenParsing m
         , Fresh m
         , Monad m
         , LookAheadParsing m
         , DeltaParsing m
         , MonadState PiState m) => m Term'
expr' = do
  pos <- position


  -- let p = newPos "" (lineNum pos) (columnNum pos)
  let p = newPos (fileName pos) (lineNum pos) (columnNum pos)

  -- Pos' p <$> (buildExpressionParser table term')


  buildExpressionParser table term'

  where table = tail
          [
            undefined

          -- , [ifix  AssocLeft "=" TyEq']
          , [Infix (resOp' "=" >>= \ws -> return $ \t -> TyEq' t ws ) AssocLeft]


          , [ifixM AssocRight "->" mkArrow]

          ]

        -- ifix  assoc op' f = Infix (resOp' op' >> return f) assoc
        -- ifix  assoc op' f = Infix (resOp' op' >>= \ws -> return f ws) assoc

        ifixM assoc op' f = Infix (resOp' op' >> f) assoc
        mkArrow  =
          do
            n <- fresh wildcardName'
            return $ \tyA tyB ->
               Pi' (bind (n, embed tyA) tyB)













data InParens = Colon Term Term | Comma Term Term | Nope Term


expProdOrAnnotOrParens :: (TokenParsing m
                          , Fresh m
                          , LookAheadParsing m
                          , DeltaParsing m
                          , MonadState PiState m) => m Term
expProdOrAnnotOrParens =
  let
    -- afterBinder picks up the return type of a pi
    -- afterBinder :: (TokenParsing m, Fresh m) => m Term
    afterBinder = do reservedOp "->"
                     rest <- expr
                     return rest

    -- before binder parses an expression in parens
    -- If it doesn't involve a colon, you get (Right tm)
    -- If it does, you get (Left tm1 tm2).  tm1 might be a variable,
    --    in which case you might be looking at an explicit pi type.
    -- beforeBinder :: (TokenParsing m, Fresh m
    --                 , LookAheadParsing m, DeltaParsing m
    --                 , MonadState PiState m) => m InParens
    beforeBinder = parens $
      choice [do e1 <- try (term >>= (\e1 -> colon >> return e1))
                 e2 <- expr
                 return $ Colon e1 e2
             , do e1 <- try (term >>= (\e1 -> comma >> return e1))
                  e2 <- expr
                  return $ Comma e1 e2
             , Nope <$> expr]
  in
    do bd <- beforeBinder
       case bd of
         Colon (Var x) a ->
           option (Ann (Var x) a)
                  (do b <- afterBinder
                      return $ Pi (bind (x,embed a) b))
         Colon a b -> return $ Ann a b
         Comma a b -> return $ Prod a b (Annot Nothing)
         Nope a    -> return $ Paren a





parenOpen' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m Token
parenOpen' = do
  runUnspaced $ symbol "("
  ws <- sliced whiteSpace
  return $ ParenOpen $ Ws $ str ws


parenClose' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m Token
parenClose' = do
  runUnspaced $ symbol ")"
  ws <- sliced whiteSpace
  return $ ParenClose $ Ws $ str ws



parens' p = do
  po <- parenOpen'
  r <- p
  pc <- parenClose'
  return (po, r, pc)



brackets' p = do
  bo <- bracketOpen'
  r <- p
  bc <- bracketClose'
  return (bo, r, bc)


colon' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m Token
colon' = do
  runUnspaced $ symbol ":"
  ws <- sliced whiteSpace
  return $ ColonTok $ Ws $ str ws


comma' :: (TokParsing m, DeltaParsing m, MonadState PiState m)
                => m Token
comma' = do
  runUnspaced $ symbol ","
  ws <- sliced whiteSpace
  return $ CommaTok $ Ws $ str ws




data InParens' =
  Colon' Term' Token Term'
  | Comma' Term' Token Term'
  | Nope' Term'


expProdOrAnnotOrParens' :: (TokenParsing m
                           , Fresh m
                           , LookAheadParsing m
                           , DeltaParsing m
                           , MonadState PiState m) => m Term'
expProdOrAnnotOrParens' =
  let
    afterBinder :: (TokenParsing m
                   , DeltaParsing m
                   , LookAheadParsing m
                   , MonadState PiState m
                   , Fresh m
                    ) => m (Ws, Term')
    afterBinder = do
      ws <- resOp' "->"
      rest <- expr'
      return (ws, rest)

    beforeBinder = do
      parens' $
        choice $ tail [
          undefined
          , do (e1,c) <- try (term' >>= (\e1 -> colon' >>= \c -> return (e1,c)))
               e2 <- expr'
               return $ Colon' e1 c e2
          , do (e1, c) <- try (term' >>= (\e1 -> comma' >>= \c -> return (e1, c)))
               e2 <- expr'
               return $ Comma' e1 c e2
          , Nope' <$> expr'
          ]
  in
    do (po, bd, pc) <- beforeBinder
       case bd of
        Colon' (Var' x ws) c a ->
           option (Ann' po (Var' x ws) c a pc)
                  (do (ws, b) <- afterBinder
                      return $ Pi' (bind (x,embed a) b))
        Colon' a c b -> return $ Ann' po a c b pc
        Comma' a c b -> return $ Prod' po a c b pc (Annot' Nothing $ Ws "")
        Nope' a    -> return $ Paren' po a pc













-- pattern :: (TokenParsing m, MonadState PiState m) =>  m Pattern
pattern :: (TokenParsing m, MonadState PiState m, DeltaParsing m) =>  m Pattern
-- Note that 'dconstructor' and 'variable' overlaps, annoyingly.
pattern =  try (PatCon <$> dconstructor <*> many arg_pattern)
       <|> atomic_pattern
  where
    arg_pattern    =  ((,Erased) <$> brackets pattern)
                  <|> ((,Runtime) <$> atomic_pattern)
    atomic_pattern = (parens pattern)
       <|> (PatVar <$> wildcard)
       <|> do t <- varOrCon
              case t of
                (Var x) -> return $ PatVar x
                (DCon c [] _) -> return $ PatCon c []
                (TCon _ []) -> fail "expected a data constructor but a type constructor was found"
                _ -> error "internal error in atomic_pattern"










-- todo: PatVar' x ws


-- pattern :: (TokenParsing m, MonadState PiState m) =>  m Pattern
pattern' :: (TokenParsing m, MonadState PiState m, DeltaParsing m) =>  m Pattern'
-- Note that 'dconstructor' and 'variable' overlaps, annoyingly.
pattern' =  try (PatCon' <$> dconstructor' <*> many arg_pattern)
       <|> atomic_pattern
  where
    arg_pattern    =  ((,Erased) <$> brackets pattern')
                  <|> ((,Runtime) <$> atomic_pattern)
    atomic_pattern =    (parens pattern')
                  -- <|> (PatVar' <$> wildcard')
                  <|> (wildcard' >>= \(tn, ws) -> return $ PatVar' tn ws)

                  <|> do t <- varOrCon'
                         case t of
                           (Var' x ws) -> return $ PatVar' x ws
                           (DCon' c [] _) -> return $ PatCon' c []
                           (TCon' _ []) -> fail "expected a data constructor but a type constructor was found"
                           _ -> error "internal error in atomic_pattern"











-- ~ idris - ParseExpr, caseOption

match :: (TokenParsing m, MonadState PiState m, Fresh m
         , LookAheadParsing m
         , DeltaParsing m) => m Match
match =
  do pat <- pattern
     reservedOp "->"
     body <- term
     return $ Match (bind pat body)



match' :: (TokenParsing m, MonadState PiState m, Fresh m
         , LookAheadParsing m
         , DeltaParsing m) => m Match'
match' =
  do pat <- pattern'
     ws <- resOp' "->"
     body <- term'
     return $ Match' (ArrowTok ws) (bind pat body)







caseExpr :: (TokenParsing m
            , Fresh m
            , MonadState PiState m
            , LookAheadParsing m
            , DeltaParsing m
            ) => m Term
caseExpr = do
    reserved "case"
    scrut <- factor
    reserved "of"

    -- pi': alts <- layout match (return ())
    -- idr: opts <- indentedBlock1 (caseOption syn)
    alts <- indentedBlock1 match

    return $ Case scrut alts (Annot Nothing)




caseExpr' :: (TokenParsing m
             , Fresh m
             , MonadState PiState m
             , LookAheadParsing m
             , DeltaParsing m
             ) => m Term'
caseExpr' = do
    res' "case"
    scrut <- factor'
    ws <- res' "of"

    -- pi': alts <- layout match (return ())
    -- idr: opts <- indentedBlock1 (caseOption syn)
    alts <- indentedBlock1 match'

    return $ Case' scrut (Of ws) alts (Annot' Nothing $ Ws "")



-- subst e0 by e1

substExpr :: (TokenParsing m
             , LookAheadParsing m
             , DeltaParsing m
             , MonadState PiState m
             , Fresh m
             ) => m Term
substExpr = do
  reserved "subst"
  a <- expr
  reserved "by"
  b <- expr
  return $ Subst a b (Annot Nothing)




substExpr' :: (TokenParsing m
             , LookAheadParsing m
             , DeltaParsing m
             , MonadState PiState m
             , Fresh m
             ) => m Term'
substExpr' = do
  ws <- res' "subst"
  a <- expr'
  ws' <- res' "by"
  b <- expr'
  return $ Subst' a b (Annot' Nothing $ Ws "")




sigmaTy :: (TokenParsing m, Fresh m
           , LookAheadParsing m
           , DeltaParsing m
           , MonadState PiState m) => m Term
sigmaTy = do
  reservedOp "{"
  x <- variable
  colon
  a <- expr
  reservedOp "|"
  b <- expr
  reservedOp "}"
  return (Sigma (bind (x, embed a) b))





natenc :: TokParsing m => m  Term
natenc =
  do n <- natural
     return $ encode n
   where encode 0 = DCon "Zero" [] natty
         encode n = DCon "Succ" [Arg Runtime (encode (n-1))] natty
         natty    = Annot $ Just (TCon "Nat" [])


-- todo:
-- think about where to put the ws:
-- as part of the DCName' or of the Annot' ?
-- for now on the (top most) DCName'

natenc' :: (TokParsing m
           , DeltaParsing m
           ) => m  Term'
natenc' = do
  n <- runUnspaced natural
  ws <- sliced whiteSpace

  -- return $ (\(DCon' n'  args (Annot' m _)) ->
  --             DCon' n'  args (Annot' m $ Ws $ str ws )) $ encode n

  return $ (\(DCon' (DCName' n' _)  args annot) ->
              DCon' (DCName' n' $ Ws $ str ws)  args annot) $ encode n


    where

   -- encode 0 = DCon' "Zero" [] natty
   encode 0 = DCon' (DCName' "Zero" $ Ws "") [] natty

   -- encode n = DCon' "Succ" [Arg' Runtime (encode (n-1))] natty
   encode n = DCon' (DCName' "Succ" $ Ws "")
              [Arg' Runtime (encode (n-1))] natty

   natty    = Annot' (Just (TCon' (TCName' "Nat" $ Ws "") [])) $ Ws ""



-- ====================
-- data types




-- eg.
-- Zero
-- Succ of (Nat)
constructorDef :: (DeltaParsing m
                  , Fresh m
                  , LookAheadParsing m
                  , MonadState PiState m
                  ) => m ConstructorDef
constructorDef = do
  pos <- position
  cname <- identifier
  args <- option Empty (reserved "of" >> telescope)


  return $ ConstructorDef (newPos
                           ""
                           (lineNum pos)
                           (columnNum pos)) cname args

  <?> "Constructor"




constructorDef' :: (DeltaParsing m
                   , Fresh m
                   , LookAheadParsing m
                   , MonadState PiState m
                   ) => m ConstructorDef'
constructorDef' = do
  pos <- position
  (cname, ws) <- id'
  -- Empty' is a telescope
  -- (oftok, args') <- option
  --                   (NoTok, Empty')
  --                   ((,) <$> runUnspaced (res' "of" >>= \s -> return $ Of s) <*> telescope')

  (oftok, args') <- option
                    (NoTok, Empty')
                    ((,) <$> (res' "of" >>= \s -> return $ Of s) <*> telescope')


  -- hm ??
  ws' <- sliced whiteSpace

  return $ ConstructorDef' (newPos
                            ""
                            (lineNum pos)
                            (columnNum pos)) (DCName' cname ws) oftok args' (Ws $ str ws')
                            -- (columnNum pos)) (DCName' cname ws) oftok args' (Ws $ "FOO")

  <?> "Constructor"









telescope :: (TokenParsing m, Fresh m, LookAheadParsing m
             , DeltaParsing m
             , MonadState PiState m
             ) => m Telescope
telescope = do
  bindings <- telebindings
  return $ foldr id Empty bindings where




telescope' :: (TokenParsing m, Fresh m, LookAheadParsing m
              , DeltaParsing m
              , MonadState PiState m
              ) => m Telescope'
telescope' = do
  -- bindings <- telebindings'
  -- let bindings' = [b | (o, b, c) <- bindings]
  -- return $ foldr id Empty' bindings' where
  bindings <- telebindings'
  -- let bindings' = [b | (o, b, c) <- bindings]
  return $ foldr id Empty' bindings where







telebindings :: (TokenParsing m, Fresh m
                , LookAheadParsing m
                , DeltaParsing m
                , MonadState PiState m)=> m [Telescope -> Telescope]
telebindings = many teleBinding
  where
    annot = do
      (x,ty) <-    try ((,) <$> varOrWildcard        <*> (colon >> expr))
                <|>    ((,) <$> (fresh wildcardName) <*> expr)
      return (Cons Runtime x ty)

    imp = do
        v <- varOrWildcard
        colon
        t <- expr
        return (Cons Erased v t)

    equal = do
        v <- variable
        reservedOp "="
        t <- expr
        return (Constraint (Var v) t)

    teleBinding =
      (    parens annot
       <|> try (brackets imp)
       <|> brackets equal) <?> "binding"


{-
  data Telescope = Empty
      | Cons   Epsilon TName Term Telescope
      | Constraint Term Term Telescope
 -}

{-
  data Telescope' =
    Empty'
    | Cons' Epsilon TName' Term' Telescope'
    | Constraint' Term' Term' Telescope'
 -}



{-
  *ReParse > :t liftM TName' $ fresh $ s2n "_"
  liftM TName' $ fresh $ s2n "_" :: Fresh m => m (Ws -> TName')
  *ReParse > :t liftM TName' $ fresh $ s2n "_"

  let tName n = TName' n (Ws "")
  tName `liftM` (fresh $ s2n "_") :: Fresh m => m TName'

  ->
  (\n -> TName' n $ Ws "") `liftM` (fresh $ s2n "_")

-}



annot' :: (TokenParsing m
          , Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m)=> m (Telescope' -> Telescope')
annot' = do
  try (do
          po <- parenOpen'
          (v, ws) <- varOrWildcard'
          colon
          ty <- expr'
          pc <- parenClose'
          return $ Cons' Runtime po v ty pc
      )
    <|>
    (do
          po <- parenOpen'
          v <- fresh wildcardName'
          ty <- expr'
          pc <- parenClose'
          return $ Cons' Runtime po v ty pc
      )




imp' :: (TokenParsing m
        , Fresh m
        , LookAheadParsing m
        , DeltaParsing m
        , MonadState PiState m
        )=> m (Telescope' -> Telescope')
imp' = do
        bo <- bracketOpen'
        (v, _) <- varOrWildcard'
        colon
        t <- expr'
        bc <- bracketOpen'
        return $ Cons' Erased bo v t bc


teleBinding' :: (TokenParsing m
        , Fresh m
        , LookAheadParsing m
        , DeltaParsing m
        , MonadState PiState m
        )=> m (Telescope' -> Telescope')
teleBinding' =
  annot'
  <|> try imp'
  <|> equal' <?> "binding"



equal' :: (TokenParsing m
        , Fresh m
        , LookAheadParsing m
        , DeltaParsing m
        , MonadState PiState m
        )=> m (Telescope' -> Telescope')
equal' = do
        bo <- bracketOpen'
        (v, ws) <- var'
        resOp' "="
        t <- expr'
        bc <- bracketClose'
        return (Constraint' bo (Var' v ws) t bc)



telebindings' :: (TokenParsing m, Fresh m
                 , LookAheadParsing m
                 , DeltaParsing m
                 , MonadState PiState m)=> m [Telescope' -> Telescope']
telebindings' = many teleBinding'

  -- where
  --   annot = do
  --     (x,ty) <-    try ((,) <$> (varOrWildcard' >>= \(v, _)-> return v)      <*> (colon >> expr'))
  --                  -- cf. above
  --                  <|>    ((,) <$> (fresh wildcardName') <*> expr')
  --     return (Cons' Runtime x ty)


  --   imp = do
  --       (v, _) <- varOrWildcard'
  --       colon
  --       t <- expr'
  --       return (Cons' Erased v t)

  --   equal = do
  --       (v, ws) <- var'
  --       resOp' "="
  --       t <- expr'
  --       return (Constraint' (Var' v ws) t)

  --   teleBinding' =
  --     (    parens annot
  --      <|> try (brackets imp)
  --      <|> brackets equal) <?> "binding"










-- dconstructor :: (TokParsing m, MonadState PiState m) => m DCName
dconstructor :: (TokParsing m, MonadState PiState m, DeltaParsing m) => m DCName
dconstructor =
  do i <- identifier
     -- cnames <- get
     st <- get
     let cnames = constr_names st



     if (i `S.member` dconNames cnames)
       then return i
       else if (i `S.member` tconNames cnames)
            then fail "Expected a data constructor, but a type constructor was found."
            else fail "Expected a constructor, but a variable was found"



dconstructor' :: (TokParsing m, MonadState PiState m, DeltaParsing m) => m DCName'
dconstructor' = do
  (i, ws) <- id'
  st <- get
  let cnames = constr_names st

  if (i `S.member` dconNames cnames)
       -- then return i
       then return $ DCName' i ws
       else if (i `S.member` tconNames cnames)
            then fail "Expected a data constructor, but a type constructor was found."
            else fail "Expected a constructor, but a variable was found"





-- ====================
-- decls
-- ie. top level declarations



sigDef :: (TokenParsing m, Fresh m
          , LookAheadParsing m
          , DeltaParsing m
          , MonadState PiState m) => m Decl
sigDef = do
  n <- try (variable >>= \v -> colon >> return v)
  ty <- expr
  return $ Sig n ty



sigDef' :: (TokenParsing m, Fresh m
           , LookAheadParsing m
           , DeltaParsing m
           , MonadState PiState m) => m Decl'
sigDef' = do
  n <- try (var' >>= \(v,ws) -> colon >> return v)
  ty <- expr'
  return $ Sig' n ty




valDef :: (TokenParsing m, Fresh m, LookAheadParsing m
          , DeltaParsing m
          ,MonadState PiState m) => m Decl
valDef = do
  n <- try (do {n <- variable; reservedOp "="; return n})
  val <- expr
  return $ Def n val



valDef' :: (TokenParsing m, Fresh m, LookAheadParsing m
           , DeltaParsing m
           ,MonadState PiState m) => m Decl'
valDef' = do
  n <- try (do {(n, ws) <- var'; resOp' "="; return n})
  val <- expr'
  return $ Def' n val





-- decl :: (TokenParsing m, Fresh m) => m Decl
-- decl = sigDef <|> valDef



-- decl :: (TokenParsing m, MonadState ConstructorNames m, Fresh m) => m Decl
decl :: (TokenParsing m, LookAheadParsing m, DeltaParsing m
        , MonadState PiState m
        , Fresh m)  => m Decl
decl = (try dataDef) <|>  sigDef <|> valDef



-- decl :: (TokenParsing m, MonadState ConstructorNames m, Fresh m) => m Decl
decl' :: (TokenParsing m, LookAheadParsing m, DeltaParsing m
         , MonadState PiState m
         , Fresh m)  => m Decl'
decl' = (try dataDef') <|>  sigDef' <|> valDef'




-- datatype decls
-- eg.
--
-- data Nat : Type where
--   Zero
--   Succ of (Nat)
--
dataDef :: (TokenParsing m, Fresh m, MonadState PiState m
           , LookAheadParsing m
           , DeltaParsing m) => m Decl
dataDef = do
  reserved "data"
  name <- identifier
  params <- telescope
  colon
  Type <- typen

  modify (\st ->
           let cnames = constr_names st
           in
            st { constr_names = cnames
                                     {
                                       tconNames = S.insert name (tconNames cnames)
                                     }
               })


  reserved "where"

  cs <- indentedBlock1 constructorDef



  forM_ cs
    (\(ConstructorDef _ cname _) ->

      modify (\st ->
               let cnames = constr_names st
               in
                st { constr_names = cnames{ dconNames = S.insert cname (dconNames cnames)}}))

  return $ Data name params cs







dataDef' :: (TokenParsing m, Fresh m, MonadState PiState m
            , LookAheadParsing m
            , DeltaParsing m) => m Decl'
dataDef' = do
  reserved "data"
  name <- identifier
  params <- telescope'
  colon
  Type' ws <- typen'

  modify (\st ->
           let cnames = constr_names st
           in
            st { constr_names = cnames
                                     {
                                       tconNames = S.insert name (tconNames cnames)
                                     }
               })


  ws' <- res' "where"

  cs <- indentedBlock1 constructorDef'



  forM_ cs
    (\(ConstructorDef' _ (DCName' cname _) oftok _ _) ->

      modify (\st ->
               let cnames = constr_names st
               in
                st { constr_names = cnames{ dconNames = S.insert cname (dconNames cnames)}}))

  return $ Data' name params (Where ws') cs









-- ====================
-- module stuff



moduleImports :: (TokenParsing m
                 , Monad m
                 , LookAheadParsing m
                 , DeltaParsing m
                 , MonadState PiState m
                 ) => m Module
moduleImports = do
  reserved "module"
  modName <- identifier
  reserved "where"
  imports <- many importDef
  return $ Module modName imports [] emptyConstructorNames




moduleDef :: (TokParsing m, MonadState PiState m
             , LookAheadParsing m
             , DeltaParsing m
             , Fresh m) =>  m Module
moduleDef = do
  reserved "module"
  modName <- identifier
  reserved "where"
  imports <- many importDef
  decls <- many decl
  st <- get
  let cnames = constr_names st
  return $ Module modName imports decls cnames




moduleDef' :: (TokParsing m, MonadState PiState m
             , LookAheadParsing m
             , DeltaParsing m
             , Fresh m) =>  m Module'
moduleDef' = do
  ws <- res' "module"
  (modName, ws') <- id'
  ws'' <- res' "where"
  imports <- many importDef'
  decls <- many decl'
  st <- get
  let cnames = constr_names st
  return $ Module' ws (MName' modName ws') (Where ws'') imports decls cnames




importDef :: (TokenParsing m, Monad m, DeltaParsing m, MonadState PiState m) => m  ModuleImport
importDef = do reserved "import" >>  (ModuleImport <$> importName)
  where importName = identifier



importDef' :: (TokenParsing m, Monad m, DeltaParsing m, MonadState PiState m)
              => m  ModuleImport'
importDef' = do
  ws <- res' "import"
  (importName, ws') <- id'
  return $ ModuleImport' ws $ MName' importName ws'





-- ====================

-- stolen from idris, ParseHelpers


pushIndent :: (MonadState PiState m, DeltaParsing m) => m ()
pushIndent = do pos <- position
                st <- get
                put (st { indent_stack = (fromIntegral (column pos) + 1) : indent_stack st })


popIndent :: MonadState PiState m => m ()
popIndent = do st <- get
               let (_ : xs) = indent_stack st
               put (st { indent_stack = xs })



indentedBlock1 :: (TokenParsing m, MonadState PiState m, DeltaParsing m,
                   LookAheadParsing m) => m a -> m [a]
indentedBlock1 p = do openBlock
                      pushIndent
                      res <- some (indented p)
                      popIndent
                      closeBlock
                      return res

indentedBlock :: (TokenParsing m, MonadState PiState m, DeltaParsing m,
                   LookAheadParsing m) => m a -> m [a]
indentedBlock p = do openBlock
                     pushIndent
                     res <- many (indented p)
                     popIndent
                     closeBlock
                     return res







lchar :: TokenParsing m => Char -> m Char
lchar = token . char


lastIndent :: MonadState PiState m => m Int
lastIndent = do st <- get
                case indent_stack st of
                  (x : _) -> return x
                  _       -> return 1


-- get current indentation
indent :: (MonadState PiState m, DeltaParsing m) => m Int
indent = liftM ((+1) . fromIntegral . column) position


indented :: (TokenParsing m, MonadState PiState m, LookAheadParsing m,
             DeltaParsing m) => m a -> m a
indented p = notEndBlock *> p <* keepTerminator



keepTerminator :: (TokenParsing m, MonadState PiState m, LookAheadParsing m,
                   DeltaParsing m) => m ()
keepTerminator =  do lchar ';'; return ()
              <|> do c <- indent;
                     l <- lastIndent
                     unless (c <= l) $ fail "not a terminator"
                     -- unless (c <= l) $ fail $ "not a terminator, current: "
                     --   ++ show c ++ " last:" ++ show l


              -- <|> do isParen <- lookAheadMatches (oneOf ")}|")
              --        isIn <- lookAheadMatches (reserved "in")
              --        unless (isIn || isParen) $ fail "not a terminator"
              <|> lookAhead eof



notEndBlock :: (MonadState PiState m, DeltaParsing m, LookAheadParsing m) => m ()
notEndBlock = do st <- get
                 case brace_stack st of
                      Just lvl : _ -> do { i <- indent
                                          ; isParen <- lookAheadMatches (char ')')
                                          ; when (i < lvl || isParen) (fail "end of block")}
                      _ -> return ()


openBlock :: (TokenParsing m, MonadState PiState m, DeltaParsing m) => m ()
openBlock =  do lchar '{'
                st <- get
                put (st { brace_stack = Nothing : brace_stack st })
             <|> do st <- get
                    lvl' <- indent
                    -- if we're not indented further, it's an empty block, so
                    -- increment lvl to ensure we get to the end
                    let lvl = case brace_stack st of
                                   Just lvl_old : _ ->
                                     if lvl' <= lvl_old then lvl_old+1 else lvl'
                                   [] -> if lvl' == 1 then 2 else lvl'
                                   _ -> lvl'
                    put (st { brace_stack = Just lvl : brace_stack st })
             <?> "start of block"


closeBlock :: (TokenParsing m, MonadState PiState m, DeltaParsing m,
               LookAheadParsing m) => m ()
closeBlock = do st <- get
                bs <- case brace_stack st of
                        []  -> eof >> return []
                        Nothing : xs -> lchar '}' >> return xs <?> "end of block"
                        Just lvl : xs -> (do i   <- indent
                                             isParen <- lookAheadMatches (char ')')
                                             isIn <- lookAheadMatches (reserved "in")
                                             if i >= lvl && not (isParen || isIn)
                                                then fail "not end of block"
                                                else return xs
                                                )
                                          <|> (do notOpenBraces
                                                  eof
                                                  return [])
                put (st { brace_stack = bs })


notOpenBraces :: MonadState PiState m => m ()
notOpenBraces = do st <- get
                   when (hasNothing $ brace_stack st) $ fail "end of input"
  where hasNothing :: [Maybe a] -> Bool
        hasNothing = any isNothing



lookAheadMatches :: (Monad m, LookAheadParsing m) => m a -> m Bool
lookAheadMatches p = do match' <- lookAhead (optional p)
                        return $ isJust match'



-- ====================
-- lineNum, columnNum stolen from idris, ParseHelpers as well




fileName :: Delta -> String
fileName (Directed fn _ _ _ _) = UTF8.toString fn
-- fileName _                     = "(interactive)"
fileName _                     = "<interactive>"



lineNum :: Delta -> Int
lineNum (Lines l _ _ _)      = fromIntegral l + 1
lineNum (Directed _ l _ _ _) = fromIntegral l + 1
lineNum _ = 0




columnNum :: Delta -> Int
columnNum pos = fromIntegral (column pos) + 1



-- Checks if application expression does not end
notEndApp :: (DeltaParsing m, MonadState PiState m) => m ()
notEndApp = do c <- indent; l <- lastIndent
               when (c <= l) (fail "terminator")
