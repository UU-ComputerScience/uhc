%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}TyCore.Coercion} import({%{EH}Base.Common},{%{EH}Base.Opts})
%%]

%%[(8 codegen) hs import(Data.Maybe)
%%]
%%[(8 codegen) hs import(EH.Util.Utils)
%%]
%%[(8 codegen) hs import(qualified {%{EH}Ty} as T)
%%]
%%[(8 codegen) hs import({%{EH}TyCore.Base})
%%]
%%[(8 codegen) hs import({%{EH}VarMp})
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[doesWhat doclatex
A Coercion represents incomplete code, in that it contains a hole to be filled in later.
Conceptually, the coercion type is defined only by:

\begin{pre}
type Coe = Expr -> Expr
\end{pre}

In the implementation here, the hole is represented by Expr_CoeArg.

We also need to manipulate coercions so more structure is encoded in @Coe@ to match on coercion variants.
In the end, a coercion is applied to a Expr to yield code,
see coeEvalOn in Core/Subst.
Additionally, this can be done in a lazy manner yielding a substitution CSubst
to be applied at the last possible moment.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The semantics of a coercion is its application to a Expr. See coeEvalOn.

%%[(8 codegen) hs export(Coe(..))
data Coe
  = Coe_Map      		!(Expr -> Expr)					-- normal, expression as function
  | Coe_C        		!Expr							-- constant
  | Coe_Compose  		!Coe !Coe						-- composition
  | Coe_App1     		!Expr !MetaVal					-- apply
  | Coe_App      		(AssocL HsName MetaVal)			-- apply n args
  | Coe_Lam      		!HsName !MetaVal !Ty			-- lambda
  | Coe_CloseExists		!TyVarId !Ty !Ty				-- closing existential
  | Coe_OpenExists		!TyVarId !Ty !Ty				-- opening existential
%%[[9
  | Coe_LamLet   		!HsName !Ty !UID				-- lambda with a let binding in the body
  | Coe_LetRec   		!ValBindL						-- let rec
  | Coe_ImplApp  		!ImplsVarId						-- implicits, for apply
  | Coe_ImplLam  		!ImplsVarId						-- implicits, for lambda
%%]]
%%]

%%[(8 codegen) hs export(coeId, mkCoe)
mkCoe :: (Expr -> Expr) -> Coe
mkCoe = Coe_Map

coeId :: Coe
coeId = Coe_C Expr_CoeArg
%%]

%%[(8 codegen) hs export(coeIsId)

coeIsId :: Coe -> Bool
coeIsId (Coe_C Expr_CoeArg) = True
-- coeIsId (Coe_Compose c1 c2 ) = coeIsId c1 && coeIsId c2
coeIsId _                   = False

instance Show Coe where
  show _ = "COE"
%%]

%%[(9 codegen) hs export(mkLamLetCoe, mkLetRecCoe)
mkLamLetCoe :: HsName -> Ty -> UID -> Coe
mkLamLetCoe = Coe_LamLet

mkLetRecCoe :: ValBindL -> Coe
mkLetRecCoe [] = coeId
mkLetRecCoe b  = Coe_LetRec b

%%]

%%[(8 codegen) hs export(mkAppCoe1With,mkAppCoe1,mkAppCoeWith,mkAppCoe)
mkAppCoe1With :: Expr -> MetaVal -> Coe
mkAppCoe1With = Coe_App1

mkAppCoe1 :: Expr -> Coe
mkAppCoe1 a = mkAppCoe1With a MetaVal_Val

mkAppCoeWith :: [(HsName,MetaVal)] -> Coe
mkAppCoeWith = Coe_App

mkAppCoe :: [Expr] -> Coe
mkAppCoe as = mkCoe (\e -> mkExprAppMeta e (metaLift as))
%%]
mkAppCoeWith :: [(Expr,MetaVal)] -> Coe
mkAppCoeWith as = mkCoe (\e -> mkExprAppMeta e as)

%%[(8 codegen) hs export(mkLamCoe1With,mkLamCoe1)
mkLamCoe1With :: HsName -> MetaVal -> Ty -> Coe
mkLamCoe1With = Coe_Lam

mkLamCoe1 :: HsName -> Ty -> Coe
mkLamCoe1 n t = mkLamCoe1With n MetaVal_Val t
%%]

%%[(8 codegen) hs export(coeCompose)
coeCompose :: Coe -> Coe -> Coe
coeCompose c1 c2
  | coeIsId c1 = c2
  | otherwise  = Coe_Compose c1 c2

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion for lamda
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A LRCoe represents a coercion in a much more finegrained manner:
- a right Coe list, a list of coercions for building the rhs side of a subsumption, which must be the lambda
- a left Coe list a list of coercions for building the lhs side of a subsumption, which must be an application or other expr using the args of the left Coe

%%[(8 codegen) hs export(LRCoeKind(..),lrcoeKindOfCoe)
data LRCoeKind = LRCoeId | LRCoeOther deriving Eq

lrcoeKindAnd :: LRCoeKind -> LRCoeKind -> LRCoeKind
lrcoeKindAnd LRCoeId LRCoeId = LRCoeId
lrcoeKindAnd _       _       = LRCoeOther

lrcoeKindOfCoe :: Coe -> LRCoeKind
lrcoeKindOfCoe c = if coeIsId c then LRCoeId else LRCoeOther
%%]

%%[(8 codegen) hs export(LRCoe(..),emptyLRCoe)
data LRCoe
  = LRCoe
      { lrcoeKind		:: LRCoeKind
      , lrcoeLeftL		:: [Coe]
      , lrcoeRightL 	:: [Coe]
      }

emptyLRCoe :: LRCoe
emptyLRCoe = LRCoe LRCoeId [] []
%%]

%%[(8 codegen) hs export(lrcoeIsId)
lrcoeIsId :: LRCoe -> Bool
lrcoeIsId c = lrcoeKind c == LRCoeId
%%]

%%[(9 codegen) hs export(mkLRCoe)
mkLRCoe :: Coe -> Coe -> LRCoe
mkLRCoe l r = LRCoe LRCoeOther [l] [r]

mkIdLRCoe' :: Coe -> Coe -> LRCoe
mkIdLRCoe' l r = LRCoe LRCoeId [l] [r]
%%]

%%[(9 codegen) hs export(mkIdLRCoeWith)
mkIdLRCoeWith :: HsName -> MetaVal -> Ty -> LRCoe
mkIdLRCoeWith n m t = mkIdLRCoe' (mkAppCoeWith [(n,m)]) (mkLamCoe1With n m t)
%%]

%%[(9 codegen) hs export(lrcoeLSingleton,lrcoeRSingleton,lrcoeLFromList,lrcoeRFromList)
lrcoeLFromList :: [Coe] -> LRCoe
lrcoeLFromList c = LRCoe LRCoeOther c []

lrcoeRFromList :: [Coe] -> LRCoe
lrcoeRFromList c = LRCoe LRCoeOther [] c

lrcoeLSingleton :: Coe -> LRCoe
lrcoeLSingleton c = LRCoe (lrcoeKindOfCoe c) [c] []

lrcoeRSingleton :: Coe -> LRCoe
lrcoeRSingleton c = LRCoe (lrcoeKindOfCoe c) [] [c]
%%]

%%[(8 codegen) hs export(lrcoeUnion)
lrcoeUnion :: LRCoe -> LRCoe -> LRCoe
lrcoeUnion (LRCoe k1 l1 r1) (LRCoe k2 l2 r2) = LRCoe (lrcoeKindAnd k1 k2) (l1 ++ l2) (r1 ++ r2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion construction for implicits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(coeImplsAppLVarMp)
coeImplsAppLVarMp :: EHCOpts -> VarMp -> Coe -> [Coe]
coeImplsAppLVarMp opts c coe
  =  case coe of
%%[[9
       Coe_ImplApp i  -> maybe [coe] (mkImplsAppCoe opts) (varmpImplsLookupCyc i c)
%%]]
       _              -> [coe]
%%]

%%[(8 codegen) hs export(coeImplsAppRVarMp)
coeImplsAppRVarMp :: VarMp -> Coe -> [Coe]
coeImplsAppRVarMp c coe
  =  case coe of
%%[[9
       Coe_ImplLam i  -> maybe [coe] (mkImplsLamCoe coeId) (varmpImplsLookupCyc i c)
%%]]
       _             -> [coe]
%%]

%%[(9 codegen) hs export(mkPoiLLamCoe,mkPoisAppCoe,mkImplsAppCoe,mkImplsLamCoe)
mkPoisAppCoe :: EHCOpts -> [PredOccId] -> [Coe]
mkPoisAppCoe opts = map (\i -> mkAppCoe1With (mkExprPrHole opts i) (MetaVal_Dict Nothing))

mkImplsAppCoe :: EHCOpts -> T.Impls -> [Coe]
mkImplsAppCoe opts = mkPoisAppCoe opts . T.implsPrIds

mkPoiLLamCoe :: Coe -> [(PredOccId,Ty)] -> [Coe]
mkPoiLLamCoe onLast poiL
  =  case map mk poiL of
       l@(_:_)            -> h ++ [t `coeCompose` onLast]
                          where (h,t) = fromJust $ initlast l
       _ | coeIsId onLast -> []
         | otherwise      -> [onLast]
  where mk (poi,ty) = mkLamCoe1With (poiHNm poi) (MetaVal_Dict Nothing) ty

mkImplsLamCoe :: Coe -> T.Impls -> [Coe]
mkImplsLamCoe onLast is = mkPoiLLamCoe onLast (zip (T.implsPrIds is) (repeat (tyErr "mkImplsLamCoe")))

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion patching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(mkLamBodyCoe)
-- adapt the last coercion of a list of coercions with additional an coercion 'onLast'
mkLamBodyCoe :: Coe -> [Coe] -> [Coe]
mkLamBodyCoe onLast l
  =  case l of
       (_:_)              -> h ++ [onLast `coeCompose` t]
                          where h = init l
                                t = last l
       _ | coeIsId onLast -> []
         | otherwise      -> [onLast]
%%]

