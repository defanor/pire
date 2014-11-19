
-- some samples for usage of the trifecta parsers
-- to be explained in more detail in the docs still
-- but usefull for experimenting / debugging etc.
-- some of the examples are now obsolete



{-

debugging
  

  ReParse
  fromSuccess $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piPrelude) top " Succ n "

  vs Parser
  runFreshM (evalStateT (runParserT (do { whiteSpace; v <- expr; eof; return v}) [] "<interactive>" " Succ n") emptyConstructorNames)

  resp. w/ prelude
  let prelude = ConstructorNames (S.fromList ["Nat"]) (S.fromList ["Zero", "Succ"])
  runFreshM (evalStateT (runParserT (do { whiteSpace; v <- expr; eof; return v}) [] "<interactive>" " Succ n") prelude)


if x occurs free, subst takes place as expected
disp $ subst (s2n "x") (Var $ s2n "z") $ fromSuccess $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piPrelude) top " \\y . plus x 2 "

Var' (s2n "x") $ Ws "{-hi-}"

:t subst (s2n "x") (\(Var' _ ws ) -> Var' (s2n "y") ws)


subst (s2n "x") (Var' (s2n "z") $ Ws "") $ Var'  (s2n "x") (Ws "{-hi-}")


subst (s2n "x") (Var' (s2n "z")) $ Var' (s2n "x") (Ws "{-hi-}")




 -}



{-
  most recent

  constructor defs + telescopes
  fromSuccess $ parseString  (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> constructorDef') piPrelude) top "    Succ of (Nat)  "

  fromSuccess $ parseString  (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> telescope') piPrelude) top "    (Nat)  "


  parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> many decl') piPrelude) "mystuff/SimpleNat.pi" >>= return .  disp . fromSuccess

  use piInit or piPrelude
  fromSuccess $ parseString  (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr') piPrelude) top "  0 "


  modules
  parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef') piInit) "mystuff/SimpleNat.pi" >>= return . disp . fromSuccess



  most recent, parsing from File
  
  fromSuccess  `liftM` parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> dataDef ) piInit) "mystuff/SimpleNat.pi"



 (disp . fromSuccess) `liftM` parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> dataDef ) piInit) "mystuff/SimpleNat.pi"

or


fromSuccess `liftM` parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> dataDef ) piInit) "mystuff/SimpleNat.pi" >>= return . disp


parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> dataDef') piInit) "mystuff/SimpleNat.pi" >>= return . disp . fromSuccess


w/ simple pretty printer - but not really pretty?
parseFromFileEx (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> many decl') piInit) "mystuff/SimpleNat.pi" >>= putStrLn  . P.ppShow . fromSuccess


 -}


{-

unspaced


identifier', identifier'' sind schon unspaced
parseString (runUnspaced $ (whiteSpace *> (many identifier''))) top "     foo    bar if a then b else c hi  bar foo "

parseString (runUnspaced $ ( (many identifier'))) top "foo    bar if a then b else c hi  bar foo "



für identifier hat man dagegen die Wahl - kein Unterschied?

parseString (runUnspaced $ runInnerParser' $ runFreshMT $ evalStateT (whiteSpace *> (many identifier)) piInit) top "     foo    bar if a then b else c hi  bar foo "

parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> (many identifier)) piInit) top "     foo    bar if a then b else c hi  bar foo "





parseTest (runInnerParser $ runUnspaced $ Unspaced identifier) "asdf     {- -}"

parseString (runUnspaced $ (whiteSpace *> Unspaced identifier)) top "asdfa    "
parseTest (runUnspaced $ Unspaced $ runFreshMT $ evalStateT identifier piInit)  "asdf   "


parseTest (runUnspaced $ Unspaced $ runFreshMT $ evalStateT (whiteSpace *> identifier) piInit)  "   asdf  "


parseString  (runUnspaced $ Unspaced $ runFreshMT $ evalStateT (whiteSpace *> identifier) piInit)  top "   asdf asdf "


parseString (runUnspaced $ Unspaced$ runFreshMT $ evalStateT (whiteSpace *> junk identifier) piInit) top  "asdf   {-  bla -}  "

-}




{-


later w/ evalStateT

disp $ fromSuccess $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit) top "  f = \\x . a  "



disp $ fromSuccess $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> valDef) piInit) top "  f =\\x . a  "



subst (s2n "f") (Var $ s2n "foo") $ fromSuccess $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit) top "  \\x . f  "


flip parseString top (runInnerParser $ runFreshMT $ flip evalStateT piInit $ whiteSpace *> expr) " df "


-}


{-


parseTest (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> identifier) piInit ) "  wow this rocks"


parseTest (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit ) "  wow this rocks"



parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit) "mystuff/Fac_.pi"


fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit) "mystuff/Fac_.pi"

fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit) "mystuff/Fac_.pi" >>= return . disp



-- mit def von data Nat...
fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> dataDef ) piInit) "mystuff/Fac_.pi"


fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> moduleDef ) piInit) "mystuff/Nat.pi" >>= return . disp



-}



{-

disp $ (subst (s2n "fac") (Var $ s2n "ffff")) $ fromSuccess $ parseExpr'  "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "

disp $ fromSuccess $ parseExpr'  "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "

disp $ fromRight' $ parseExpr  "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "


-}



{-

fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT (whiteSpace *> valDef)) "mystuff/Fac_.pi"

(disp . fromJust) `liftM` parseFromFile (runInnerParser $ runFreshMT (whiteSpace *> valDef)) "mystuff/Fac_.pi"
resp
fromJust `liftM` parseFromFile (runInnerParser $ runFreshMT (whiteSpace *> valDef)) "mystuff/Fac_.pi" >>= return . disp



parseFromFile (runInnerParser $ runFreshMT (whiteSpace *> valDef)) "mystuff/Fac_.pi"

parseTest (runInnerParser $ runFreshMT (whiteSpace *> valDef)) " f =  \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "




parseString (runInnerParser $ runFreshMT (whiteSpace *> valDef)) (Columns 0 0) " f =  \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "


disp $ fromSuccess $ parseString (runInnerParser $ runFreshMT (whiteSpace *> valDef)) (Columns 0 0) " f =  \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "

later w/ evalStateT
disp $ fromSuccess $ parseString (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit) top "  f =\\x . a  "



disp $ fromSuccess $ parseString (runInnerParser $ runFreshMT (whiteSpace *> valDef)) (Columns 0 0) " f =  \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "





-}



{-


parseTest (runInnerParser $ runFreshMT (whiteSpace *> expr)) "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "



parseTest (runInnerParser $ runFreshMT $ runF (whiteSpace *> identifier)) "wow this rocks"



w/ wo/ runF

parseTest (runInnerParser $ runFreshMT $ whiteSpace *> (many $ string "blah")) "  blah blah oh too {- -} bad"

parseTest (runInnerParser $ runFreshMT $ runF $ whiteSpace *> (many $ string "blah")) "  blahblah oh too {- -} bad"

parseTest (runInnerParser $ runFreshMT $ runF $ whiteSpace *> (many expr)) "   blahblah oh too {- -} bad"
parseTest (runInnerParser $ runFreshMT $ runF $ whiteSpace *>       expr) "   blahblah oh too {- -} bad"


parseTest (runInnerParser $ runFreshMT $ runF $ whiteSpace *> identifier) "wow this rocks"



parseTest (runInnerParser $ runFreshMT $ whiteSpace *> expr) "   blahblah oh too {- -} bad"





parseTest (runInnerParser $ runFreshMT $ evalStateT (whiteSpace *> expr) piInit ) "   \\n . if nat_leq n 0 then 1 else mult n (fac (minus n 1))  "



-}


