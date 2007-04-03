-----------------------------------------------------------------------------
-- StaticThih:		Static environment for Thih
-- 
-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
-- 
-----------------------------------------------------------------------------

module StaticThih(module StaticPrelude,
                  module StaticList,
                  module StaticMonad,
                  module StaticThih) where
import Static
import StaticPrelude
import StaticList
import StaticMonad

-----------------------------------------------------------------------------
-- Thih types:

tId = TAp tList tChar

tKind = TCon (Tycon "Kind" Star)
starCfun = "Star" :>: (Forall [] ([] :=> tKind))
kfunCfun = "Kfun" :>: (Forall [] ([] :=> (tKind `fn` tKind `fn` tKind)))

tType = TCon (Tycon "Type" Star)
tVarCfun = "TVar" :>: (Forall [] ([] :=> (tTyvar `fn` tType)))
tConCfun = "TCon" :>: (Forall [] ([] :=> (tTycon `fn` tType)))
tApCfun = "TAp" :>: (Forall [] ([] :=> (tType `fn` tType `fn` tType)))
tGenCfun = "TGen" :>: (Forall [] ([] :=> (tInt `fn` tType)))

tTyvar = TCon (Tycon "Tyvar" Star)
tyvarCfun = "Tyvar" :>: (Forall [] ([] :=> (tId `fn` tKind `fn` tTyvar)))

tTycon = TCon (Tycon "Tycon" Star)
tyconCfun = "Tycon" :>: (Forall [] ([] :=> (tId `fn` tKind `fn` tTycon)))

tSubst = TAp tList (TAp (TAp tTuple2 tTyvar) tType)

tQual = TCon (Tycon "Qual" (Kfun Star Star))
qualifyCfun
 = ":=>" :>: (Forall [Star]
               ([] :=> 
                (TAp tList tPred `fn` TGen 0 `fn` TAp tQual (TGen 0))))

tPred = TCon (Tycon "Pred" Star)
isInCfun = "IsIn" :>: (Forall [] ([] :=> (tId `fn` tType `fn` tPred)))

tClass
 = TAp (TAp tTuple2 (TAp tList (TAp tList tChar))) (TAp tList (TAp tQual tPred))

tInst = TAp tQual tPred

tClassEnv = TCon (Tycon "ClassEnv" Star)
classEnvCfun
 = "ClassEnv" :>: (Forall []
                    ([] :=> 
                     ((tId `fn` TAp tMaybe tClass) `fn` TAp tList tType `fn` tClassEnv)))
classesSfun
 = "classes" :>: (Forall []
                   ([] :=> 
                    (tClassEnv `fn` tId `fn` TAp tMaybe tClass)))
defaultsSfun
 = "defaults" :>: (Forall []
                    ([] :=> 
                     (tClassEnv `fn` TAp tList tType)))

tEnvTransformer
 = tClassEnv `fn` TAp tMaybe tClassEnv

tScheme
 = TCon (Tycon "Scheme" Star)
forallCfun
 = "Forall" :>: (Forall []
                  ([] :=> 
                   (TAp tList tKind `fn` TAp tQual tType `fn` tScheme)))

tAssump
 = TCon (Tycon "Assump" Star)
assumeCfun
 = ":>:" :>: (Forall []
               ([] :=> 
                (tId `fn` tScheme `fn` tAssump)))

tTI
 = TCon (Tycon "TI" (Kfun Star Star))
tICfun
 = "TI" :>: (Forall [Star]
              ([] :=> 
               ((tSubst `fn` tInt `fn` TAp (TAp (TAp tTuple3 tSubst) tInt) (TGen 0)) `fn` TAp tTI (TGen 0))))

tInfer a b
 = tClassEnv `fn` TAp tList tAssump `fn` a `fn` TAp tTI (TAp (TAp tTuple2 (TAp tList tPred)) b)

tLiteral = TCon (Tycon "Literal" Star)
litIntCfun = "LitInt" :>: (Forall [] ([] :=> (tInteger `fn` tLiteral)))
litCharCfun = "LitChar" :>: (Forall [] ([] :=> (tChar `fn` tLiteral)))
litRatCfun = "LitRat" :>: (Forall [] ([] :=> (tRational `fn` tLiteral)))
litStrCfun = "LitStr" :>: (Forall [] ([] :=> (tString `fn` tLiteral)))

tPat = TCon (Tycon "Pat" Star)
pVarCfun = "PVar" :>: (Forall [] ([] :=> (tId `fn` tPat)))
pWildcardCfun = "PWildcard" :>: (Forall [] ([] :=> tPat))
pAsCfun = "PAs" :>: (Forall [] ([] :=> (tId `fn` tPat `fn` tPat)))
pLitCfun = "PLit" :>: (Forall [] ([] :=> (tLiteral `fn` tPat)))
pNpkCfun = "PNpk" :>: (Forall [] ([] :=> (tId `fn` tInteger `fn` tPat)))
pConCfun = "PCon" :>: (Forall [] ([] :=> (tAssump `fn` TAp tList tPat `fn` tPat)))

tExpr = TCon (Tycon "Expr" Star)
varCfun = "Var" :>: (Forall [] ([] :=> (tId `fn` tExpr)))
litCfun = "Lit" :>: (Forall [] ([] :=> (tLiteral `fn` tExpr)))
constCfun = "Const" :>: (Forall [] ([] :=> (tAssump `fn` tExpr)))
apCfun = "Ap" :>: (Forall [] ([] :=> (tExpr `fn` tExpr `fn` tExpr)))
letCfun = "Let" :>: (Forall [] ([] :=> (tBindGroup `fn` tExpr `fn` tExpr)))

tAlt = TAp (TAp tTuple2 (TAp tList tPat)) tExpr

tAmbiguity = TAp (TAp tTuple2 tTyvar) (TAp tList tPred)

tExpl
 = TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))

tImpl
 = TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))

tBindGroup
 = TAp (TAp tTuple2 (TAp tList (TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) (TAp tList (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr)))))

tProgram
 = TAp tList (TAp (TAp tTuple2 (TAp tList (TAp (TAp (TAp tTuple3 (TAp tList tChar)) tScheme) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))) (TAp tList (TAp tList (TAp (TAp tTuple2 (TAp tList tChar)) (TAp tList (TAp (TAp tTuple2 (TAp tList tPat)) tExpr))))))

-----------------------------------------------------------------------------

thihClasses =   addClass cHasKind []
            <:> addClass cTypes []
            <:> addClass cInstantiate []
            <:> instances instsThih

instsThih
 = [mkInst [] ([] :=> isIn1 cEq tKind),
    mkInst [] ([] :=> isIn1 cEq tType),
    mkInst [] ([] :=> isIn1 cEq tTyvar),
    mkInst [] ([] :=> isIn1 cEq tTycon),
    mkInst [Star] ([isIn1 cEq (TGen 0)] :=> isIn1 cEq (TAp tQual (TGen 0))),
    mkInst [] ([] :=> isIn1 cEq tPred),
    mkInst [] ([] :=> isIn1 cEq tScheme),
    mkInst [] ([] :=> isIn1 cMonad tTI),
    mkInst [] ([] :=> isIn1 cHasKind tTyvar),
    mkInst [] ([] :=> isIn1 cHasKind tTycon),
    mkInst [] ([] :=> isIn1 cHasKind tType),
    mkInst [] ([] :=> isIn1 cTypes tType),
    mkInst [Star] ([isIn1 cTypes (TGen 0)] :=> 
      isIn1 cTypes (TAp tList (TGen 0))),
    mkInst [Star] ([isIn1 cTypes (TGen 0)] :=> 
      isIn1 cTypes (TAp tQual (TGen 0))),
    mkInst [] ([] :=> isIn1 cTypes tPred),
    mkInst [] ([] :=> isIn1 cTypes tScheme),
    mkInst [] ([] :=> isIn1 cTypes tAssump),
    mkInst [] ([] :=> isIn1 cInstantiate tType),
    mkInst [Star] ([isIn1 cInstantiate (TGen 0)] :=> 
      isIn1 cInstantiate (TAp tList (TGen 0))),
    mkInst [Star] ([isIn1 cInstantiate (TGen 0)] :=> 
      isIn1 cInstantiate (TAp tQual (TGen 0))),
    mkInst [] ([] :=> isIn1 cInstantiate tPred)]

cHasKind = "HasKind"
kindMfun
 = "kind" :>: (Forall [Star]
                ([isIn1 cHasKind (TGen 0)] :=> (TGen 0 `fn` tKind)))

cTypes = "Types"

applyMfun
 = "apply" :>: (Forall [Star]
                 ([isIn1 cTypes (TGen 0)] :=> 
                  (tSubst `fn` TGen 0 `fn` TGen 0)))
tvMfun
 = "tv" :>: (Forall [Star]
              ([isIn1 cTypes (TGen 0)] :=> 
               (TGen 0 `fn` TAp tList tTyvar)))

cInstantiate = "Instantiate"

instMfun
 = "inst" :>: (Forall [Star]
                ([isIn1 cInstantiate (TGen 0)] :=> 
                 (TAp tList tType `fn` TGen 0 `fn` TGen 0)))

-----------------------------------------------------------------------------
