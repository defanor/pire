
{-# LANGUAGE TemplateHaskell,
             FlexibleInstances,
             MultiParamTypeClasses,
             FlexibleContexts,
             UndecidableInstances,
             ViewPatterns,
             EmptyDataDecls #-}

{-# OPTIONS_GHC -Wall -fno-warn-unused-matches -fno-warn-orphans #-}



module ReSyntax where



import Unbound.LocallyNameless hiding (Data,Refl)

-- SourcePos
import Text.ParserCombinators.Parsec.Pos


-- for Delta
-- import Text.Trifecta.Delta



-- Epsilon
import Syntax


-- import Text.ParserCombinators.Parsec.Pos


newtype Ws =
  Ws String
  deriving (Show, Eq)



data Token =
  Reserved Ws
  | Op Ws
  | Id Ws
  | BracketOpen Ws
  | BracketClose Ws
  | ParenOpen Ws
  | ParenClose Ws
  | ColonTok Ws
  | CommaTok Ws
  | Of Ws
  | ArrowTok Ws
  | Where Ws
  | NoTok
  deriving (Show, Eq)





type TName' = Name Term'


data Term' =
     Type' Ws
   | Var' TName' Ws

   | BtVar' Token TName' Ws Token
     

   | Lam'       Ws (Bind (TName', Embed Annot') Term')

   | ErasedLam' Ws Token (Bind (TName', Embed Annot') Term') Token






   | App' Term' Term'

   | Pi' (Bind (TName', Embed Term') Term')

     
     -- annotation in parens, 
   | Ann' Token Term' Token Term' Token

     -- ( a , b )
   | Prod' Token Term' Token Term' Token Annot'

     
     -- ( t ) -- parens may contain ws
   | Paren' Token Term' Token



{-| Pos' SourcePos Term'       -- ^ marked source position, for error messages -}


   | TrustMe' Annot'


   | TyUnit' Ws
   | LitUnit' Ws


   | TyBool' Ws
   | LitBool' Bool Ws



   | If' Ws Term' Ws Term' Ws Term' Annot'



-- sigma type `{ x : A | B }`
--    | Sigma (Bind (TName, Embed Term) Term)

     


-- elimination form  `pcase p of (x,y) -> p`
--    | Pcase Term (Bind (TName, TName) Term) Annot


     -- Ws after let, name, "=", and "in"
   | Let' Ws Ws Ws Ws (Bind (TName', Embed Term') Term')


--    -- propositional equality
   
   | TyEq' Term' Ws Term'


   | Refl' Annot'



   | Subst' Term' Term' Annot'



--    | Contra Term Annot  -- ^ witness to an equality contradiction



--    | ErasedPi  (Bind (TName, Embed Term) Term)   -- ^ function type
--    | ErasedApp Term Term                         -- ^ application


   | TCon' TCName' [Term']
   | DCon' DCName' [Arg'] Annot'

   -- case t of matches annot 
   | Case' Term' Token [Match'] Annot'


     deriving (Show)


data  Annot' = Annot' (Maybe Term') Ws deriving Show


-- in token the arrow
data Match' = Match' Token (Bind Pattern' Term') deriving (Show)


-- deriving (Show, Eq)

data Pattern' = 
   PatCon' DCName' [(Pattern', Epsilon)]
 | PatVar' TName' Ws
  deriving (Show)


--------------------------------------------------



data MName' =  MName' String Ws
             deriving (Show, Eq)


data DCName' =  DCName' String Ws
             deriving (Show)


data TCName' =  TCName' String Ws
             deriving (Show)

--------------------------------------------------


data Module' = Module' { moduleWs'           :: Ws
                       , moduleName'         :: MName'
                       , moduleWhere'        :: Token
                       , moduleImports'      :: [ModuleImport']
                       , moduleEntries'      :: [Decl']
                       , moduleConstructors' :: ConstructorNames
                       }
               
  deriving (Show)

-- token Where           
data ModuleImport' = ModuleImport' Ws MName'
  deriving (Show,Eq)




data Decl' =
  Sig'     TName'  Term'
  | Def'     TName'  Term'
  | RecDef' TName' Term'

  -- token where 
  | Data'    TCName Telescope' Token [ConstructorDef']

  | DataSig' TCName Telescope'

  deriving (Show)




data ConstructorDef' =
  -- token tele: Of ... | NoToken Empty'
  ConstructorDef' SourcePos DCName' Token Telescope' Ws

  deriving (Show)



-------------
-- * Telescopes
-------------

data Telescope' =
  Empty'

  | Cons' Epsilon Token TName' Term' Token Telescope'

  | Constraint' Token Term' Term' Token Telescope'
  deriving (Show)


-- instance Show (Telescope' -> Telescope')


-- Epsilon from Syntax


data Arg'  = Arg' Epsilon Term' deriving (Show)



-------------
-- aux
-------------



wildcardName' :: TName'
wildcardName' = s2n "_"



isNumeral' :: Term' -> Maybe (Int, Ws)
-- isNumeral' (Pos' _ t) = isNumeral' t
-- isNumeral' (Paren' t) = isNumeral' t



isNumeral' (DCon' (DCName' c ws) [] _) | c== "Zero" = Just (0, ws)

isNumeral' (DCon' (DCName' c ws) [Arg' _ t] _) | c ==  "Succ" =
  do 
     (n, _) <- isNumeral' t ; 
     return (n+1, ws)

isNumeral' _ = Nothing




-------------
-- ...
-------------



derive $ tail [
       undefined

       , ''Ws
       , ''Arg'

       , ''Token

       , ''Term'
       , ''Annot'
       , ''Telescope'

       , ''Pattern'
       , ''Match'

       , ''DCName'
       , ''TCName'
       , ''MName'
       ]


instance Alpha Ws

instance Alpha Token

instance Alpha Term'

instance Alpha Match'

instance Alpha Telescope'

instance Alpha Arg'

-- instance Alpha Annot'

instance Alpha Pattern'

instance Alpha DCName'
instance Alpha TCName'
instance Alpha MName'


---------------------



instance Alpha Annot' where
    -- override default behavior so that type annotations are ignored
    -- when comparing for alpha-equivalence
    aeq' _ _ _ = True



-- The subst class derives capture-avoiding substitution
-- It has two parameters because the sort of thing we are substiting
-- for may not be the same as what we are substituting into:

-- class Subst b a where
--    subst  :: Name b -> b -> a -> a       -- single substitution
--    substs :: [(Name b, b)] -> a -> a     -- multiple substitution


instance Subst Term' Term' where
  isvar (Var' x _) = Just (SubstName x)
  isvar _ = Nothing


-- instance Subst (Ws -> Term') (Ws -> Term') where
--   isvar (Var' x) = Just (SubstName x)
--   isvar _ = Nothing


-- instance Subst (Ws -> Term') Term'
-- instance Subst (Ws -> Term') Match'
-- instance Subst (Ws -> Term') Arg'
-- instance Subst (Ws -> Term') DCName'
-- instance Subst (Ws -> Term') TCName'
-- instance Subst (Ws -> Term') Annot'
-- instance Subst (Ws -> Term') Pattern'
-- instance Subst (Ws -> Term') Token
-- instance Subst (Ws -> Term') Epsilon
-- instance Subst (Ws -> Term') Ws



instance Subst Term' Epsilon

instance Subst Term' Ws

instance Subst Term' TCName'
instance Subst Term' DCName'

instance Subst Term' Match'

instance Subst Term' Token


instance Subst Term' Pattern'
-- instance Subst Term Telescope

instance Subst Term' Arg'
-- instance Subst Term ConstructorDef

instance Subst Term' Annot'
