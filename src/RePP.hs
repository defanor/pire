

-- {-# LANGUAGE TypeSynonymInstances #-}


-- {-# LANGUAGE ExistentialQuantification #-}


{-# LANGUAGE FlexibleInstances #-}


{-# LANGUAGE UndecidableInstances #-}


{-# LANGUAGE FlexibleContexts #-}


{-# LANGUAGE ViewPatterns #-}


-- {-# LANGUAGE DefaultSignatures #-}



-- -- {-# OPTIONS_GHC -Wall -fno-warn-unused-matches -fno-warn-name-shadowing #-}





{-# OPTIONS_GHC -fno-warn-orphans #-}



module RePP  where


import PrettyPrint



import Syntax

import ReSyntax



-- in particular as well: Alpha, lunbind
import Unbound.LocallyNameless hiding (empty,Data,Refl)


import Control.Monad.Reader

import Text.PrettyPrint as PP



import Control.Applicative ((<$>), (<*>))



-- some helper
disp' x = text "<<" <> disp x <> text ">>"



instance Disp Ws where


-- don't want to rely on "Display (a, Epsilon)" here
-- because of the brackets
instance Disp (TName', Epsilon) where
  disp (t, ep) = disp  t


  -- disp (t, ep) = display'(t, ep)
  --   where  display (t, ep) = bindParens ep <$> display t



--   disp (n, eps) = parens $ (disp n) <> comma <+> disp eps
--   disp (n, eps) = parens $ (disp n) <> comma <+> disp eps

instance Disp (TName', Ws) where
  disp (t, ws) = disp t <> disp ws
  
instance Disp (TName, Epsilon) where

instance Disp Term' where

instance Disp Token


instance Disp Telescope'



-------------------------------------------------------------------------
-- Modules and Decls
-------------------------------------------------------------------------

instance Disp MName' where

instance Disp Module' where
  disp m = text "module" 
           <> (disp $ moduleWs' m)
           <> disp (moduleName' m)
           <> disp (moduleWhere' m) $$
           vcat (map disp (moduleImports' m)) $$
           disp (moduleEntries' m)


instance Disp ModuleImport' where
  disp (ModuleImport' ws i) = text "import" <> disp ws <> disp i


instance Disp [Decl'] where
  disp = hcat . map disp


instance Disp Decl' where

  disp (Def' n term) = disp n <+> text "=" <+> disp term

  disp (RecDef' n r) = disp (Def' n r)

  disp (Sig' n ty) =
        disp n <+> text ":" <+> disp ty


  disp (Data' n params wheretok constructors) =
    text "data" <+> disp n <+> disp params
           <+> colon <+> text "Type"
           <+> disp wheretok <>
           (hcat $ map disp constructors)




  disp (DataSig' t delta) =
        text "data" <+> disp t <+> disp delta <+> colon
    <+> text "Type"




instance Disp ConstructorDef' where
  disp (ConstructorDef' _ c _ Empty' ws) = disp c <> disp ws
  disp (ConstructorDef' _ c oftok tele ws)  = disp c <> disp oftok <> disp tele <> disp ws



instance Disp DCName' where


-- ====================
-- Display stuff


-- needs aeq for ... in ReSyntax


instance Display Ws where
  display (Ws s) = display s


instance Display Token  where

  display (BracketOpen ws) = do
    ws' <- display ws
    return $ text "[" <>  ws'

  display (BracketClose ws) = do
    ws' <- display ws
    return $ text "]" <>  ws'

  display (ParenOpen ws) = do
    ws' <- display ws
    return $ text "(" <>  ws'

  display (ParenClose ws) = do
    ws' <- display ws
    return $ text ")" <>  ws'


  display (ColonTok ws) = do
    ws' <- display ws
    return $ text ":" <>  ws'


  display (CommaTok ws) = do
    ws' <- display ws
    return $ text "," <>  ws'


  display (Of ws) = do
    ws' <- display ws
    return $ text "of" <>  ws'


  display (ArrowTok ws) = do
    ws' <- display ws
    return $ text "->" <>  ws'


  display (Where ws) = do
    ws' <- display ws
    return $ text "where" <>  ws'


  display NoTok = return empty




  display _ = do
    return $ text "<<display token>>"





instance Display Term' where


  display (isNumeral' -> Just (i, ws)) =
   display i >>= \di ->
   display ws >>= \dws ->
   return $ di <> dws


  display (Type' ws) =
          display ws >>= \ws' ->
          return $ text "Type" <> ws'



  display (Var' n ws) =
    display n >>= \dn ->
    display ws >>= \dw ->
    return $ dn <> dw

  -- bracket open, name, whitespace, bracket close
  display (BtVar' bo n ws bc) =
    display bo >>= \dbo ->
    display n >>= \dn ->
    display ws >>= \dws ->
    display bc >>= \dbc ->
    return $ dbo <> dn <> dws <> dbc



  display (TrustMe' ma)  = do
    da <- display ma
    return $ text "TRUSTME" <> da


  display (Paren' po t pc)  = do
    dpo <- display po
    dt <- display t
    dpc <- display pc
    return $ dpo <> dt <> dpc


  -- paren open, a, colon, b, paren close
  display (Ann' po a c b pc)    = do
    dpo <- display po
    da <- display a
    dc <- display c
    db <- display b
    dpc <- display pc
    return $ dpo <> da <> dc <> db <> dpc


  display (Prod' po a c b pc ann) = do
    dpo <- display po
    da <- display a
    dc <- display c
    db <- display b
    dpc <- display pc
    dann <- display ann
    return $ dpo <> da <> dc <> db <> dpc <> dann



  display (App' f x) = do
     df <- display f
     dx <- display x
     -- return $ wrapf' f df <> wraparg' x dx
     return $ wrapf' f df <> dx



  -- needs reworking
  display (Pi' bnd) = do
    lunbind bnd $ \((n,a), b) -> do
      da <- display (unembed a)
      dn <- display n
      db <- display b
      let lhs = if (n `elem` fv b) then
                  parens (dn <+> colon <+> da)
                else
                  wraparg' (unembed a) da
      return $ lhs <+> text "->" <+> db

    
  display (TyBool' ws) = display ws >>= \ws' -> return $ text "Bool"  <> ws'


  display (LitBool' b ws) = display ws >>= \ws' ->
    return $ if b then text "True" <> ws' else text "False" <> ws'



  -- maybe use convenience fn display (Then...)
  display (If' i a t b e c ann) = do
    di <- display i
    da <- display a
    dt <- display t
    db <- display b
    de <- display e
    dc <- display c
    dann <- display ann
    return $ text "if" <> di
      <> da
      <> text "then" <> dt
      <> db
      <> text "else" <> de
      <> dc
      <+> dann




  -- ws after "let", name, "=", and "in"
  display (Let' ws ws' ws'' ws''' bnd) = do
    lunbind bnd $ \ ((x,a) , b) -> do
     dws <- display ws
     dws' <- display ws'
     dws'' <- display ws''
     dws''' <- display ws'''
     da <- display (unembed a)
     dx <- display x
     db <- display b
     return $  hcat [text "let" <> dws <> dx <> dws'
                    <> text "=" <> dws'' <> da
                    <> text "in" <> dws''',
                    db]



  display a@(Lam' ws _) = do
    dws <- display ws
    (binds, body) <- gatherBinders' a
    return $ text "\\" <> dws <> hcat binds <> text ". " <> body



  display a@(ErasedLam' ws _ _ _) = do
    (binds, body) <- gatherBinders' a
    dws <- display ws
    return $ hang (text "\\" <> dws <> hcat binds <> text ".") 2 body



  display (TCon' n args) = do
    dn <- display n
    dargs <- mapM display args
    let wargs = zipWith wraparg' args dargs
    return $ dn <+> hsep wargs

  display (DCon' n args annot) = do
    dn     <- display n
    dargs  <- mapM display args
    dannot <- display annot
    return $ dn <+> hsep dargs <+> dannot



  display (Case' scrut oftok alts annot) = do
     dscrut <- display scrut
     dof <- display oftok
     dalts <- mapM display alts
     dannot <- display annot
     return $ text "case" <+> dscrut <> dof <> (hcat $ dalts) <> dannot



  display (TyEq' a ws b)   = do
      da <- display a
      dws <- display ws
      db <- display b
      return $ da <> text "=" <> dws <> db


  
  -- details need reworking
     
  display (Subst' a b annot) = do
      da  <- display a
      db  <- display b
      dat <- display annot
      return $ hcat [text "subst" <+> da,
                     text "by" <+> db,
                     dat]



  display (Refl' mty) = do
    da <- display mty 
    return $ text "refl" <+> da




  display _ = return $ text "<<display Term'>>"




instance Display Match' where
  display (Match' arrow bd) =
    lunbind bd $ \ (pat, ubd) -> do
      dpat <- display pat
      dubd <- display ubd
      darr <- display arrow
      return $ dpat <> darr <> dubd


instance Display Pattern' where
  display (PatCon' c []) = (display c)

  display (PatCon' c args) =
    ((<>) <$> (display c) <*> (hcat <$> (mapM display args)))
  display (PatVar' x ws) = 
     display x >>= \dx -> display ws >>= \dw -> return $ dx <> dw



instance Display DCName' where
  display (DCName' n ws) = do
    dn <- display n
    dws <- display ws
    return $ dn <> dws


instance Display TCName' where
  display (TCName' n ws) = do
    dn <- display n
    dws <- display ws
    return $ dn <> dws


instance Display MName' where
  display (MName' n ws) = do
    dn <- display n
    dws <- display ws
    return $ dn <> dws






-- needs DispInfo(..) exported from PrettyPrint

-- todo ws
instance Display Annot' where
  display (Annot' Nothing ws) = display ws

  display (Annot' (Just x) ws) = do
    st <- ask
    if (showAnnots st) then
      do
        dx <- display x
        dws <- display ws
        return $ (text ":") <+> dx <> dws
      else return $ empty




instance Display Arg' where
  display arg@(Arg' ep t) = do
    st <- ask
    let annotParens = if showAnnots st
                      then mandatoryBindParens'
                      else bindParens'

    let wraparg'' (Arg' ep' x) = case x of
              Var' _ _    -> bindParens' ep'
              BtVar' _ _ _ _  -> bindParens' ep'
              TCon' _ []   -> bindParens' ep'
              Type' _     -> bindParens' ep'
              TyUnit' _   -> bindParens' ep'
              LitUnit' _  -> bindParens' ep'
              TyBool' _   -> bindParens' ep'
              LitBool' _ _ -> bindParens' ep'
              -- Sigma' _     -> bindParens' ep'

              -- Pos' _ a     -> wraparg' (Arg ep' a)

              DCon' _ [] _ -> annotParens ep'
              -- Prod' _ _ _  -> annotParens ep'
              TrustMe' _    -> annotParens ep'
              -- Refl' _      -> annotParens ep'

              _           -> mandatoryBindParens' ep'

    wraparg'' arg <$> display t




--------------------------------------------------


-- wo/ brackets
bindParens' :: Epsilon -> Doc -> Doc
bindParens' Runtime    d = d
-- bindParens' Erased     d = brackets d
bindParens' Erased     d = brackets d



mandatoryBindParens' :: Epsilon -> Doc -> Doc
mandatoryBindParens' Runtime d = parens d
mandatoryBindParens' Erased  d = brackets d



-- deciding whether to add parens to the func of an application
wrapf' :: Term' -> Doc -> Doc
wrapf' f = case f of
  Var' _ _        -> id
  BtVar' _ _ _ _ -> id
  App' _ _       -> id
  -- ErasedApp' _ _ -> id

  -- Paren' _       -> id
  -- Pos' _ a       -> wrapf' a
  TrustMe' _      -> id
  DCon' _ _ _       -> id
  TCon' _ _       -> id
  _             -> parens


-- deciding whether to add parens to the arg of an application
wraparg' :: Term' -> Doc -> Doc
wraparg' x = case x of
  Var' _  _      -> id
  BtVar' _ _ _ _   -> id
  Type'  _      -> id
  TyUnit' _     -> id
  LitUnit' _    -> id
  TyBool' _     -> id
  LitBool' _ _  -> id
  -- Sigma' _    -> id
  TrustMe' _  -> id
  TCon' _ []    -> id
  DCon' _ [] _  -> id

  -- Refl' _      -> id

  -- Pos' _ a     -> wraparg' a
  _           -> parens








instance Display Telescope' where
  -- display (Empty' ws) = display ws >>= \ws' -> return $ empty <> ws'
  display Empty'= return empty
  display (Constraint' tok1 t1 t2 tok2 tele) = do
      dtok1 <- display tok1
      dt1 <- display t1
      dt2 <- display t2
      dtok2 <- display tok2
      dtele <- display tele
      -- return $ brackets (dt1 <+> char '=' <+> dt2) <+> dtele
      return $ dtok1 <> dt1 <+> char '=' <+> dt2 <> dtok2 <+> dtele
  display (Cons' ep tok1 n ty tok2 tele) = do
      dtok1 <- display tok1
      dn <- display n
      dty <- display ty
      dtok2 <- display tok2
      dtele <- display tele
      -- return $ mandatoryBindParens' ep (dtok1 <> dn <+> colon <+> dty <> dtok2) <+> dtele
      return $ dtok1 <> dn <+> colon <+> dty <> dtok2 <+> dtele



gatherBinders' :: Term' -> M ([Doc], Doc)
-- need yet treat ws
gatherBinders' (Lam' _ b) =
   lunbind b $ \((n, unembed->ma), body) -> do
      dn <- display n
      dt <- display ma
      let db = if isEmpty dt then dn else (dn <> dt)
      (rest, body') <- gatherBinders' body
      return $ (db : rest, body')



-- no brackets
gatherBinders' (ErasedLam' _ bo b bc) =
   lunbind b $ \((n,unembed->ma), body) -> do
     dbo <- display bo
     dn <- display n
     dbc <- display bc
     dt <- display ma
     (rest, body') <- gatherBinders' body
     return $ ((dbo <> dn <> dt <> dbc ) : rest, body')


gatherBinders' body = do
  db <- display body
  return ([], db)



-- instance Disp [Term] where
--   disp = vcat . map disp

-- instance Disp [(Name Term,Term)] where
--   disp = vcat . map disp

-- instance Disp (TName,Term) where
--   disp (n,t) = parens $ (disp n) <> comma <+> disp t

