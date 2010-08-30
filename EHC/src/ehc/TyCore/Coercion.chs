%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) hs module {%{EH}TyCore.Coercion} import({%{EH}Base.Common},{%{EH}Base.Opts})
%%]

%%[(8 codegen tycore) hs import(Data.Maybe)
%%]
%%[(8 codegen tycore) hs import(EH.Util.Utils)
%%]
%%[(8 codegen tycore) hs import(qualified {%{EH}Ty} as T)
%%]
%%[(8 codegen tycore) hs import({%{EH}TyCore.Base})
%%]
%%[(8 codegen tycore) hs import({%{EH}AbstractCore})
%%]
%%[(8 codegen tycore) hs import({%{EH}VarMp})
%%]

%%[(8 codegen tycore) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The semantics of a coercion is its application to a Expr. See coeEvalOn.

%%[(8 codegen tycore) hs export(Coe,Coe'(..))
type Coe = Coe' Expr MetaVal ValBind ValBind Ty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion for lamda
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A LRCoe represents a coercion in a much more finegrained manner:
- a right Coe list, a list of coercions for building the rhs side of a subsumption, which must be the lambda
- a left Coe list a list of coercions for building the lhs side of a subsumption, which must be an application or other expr using the args of the left Coe

%%[(8 codegen tycore) hs export(LRCoeKind(..),lrcoeKindOfCoe)
data LRCoeKind = LRCoeId | LRCoeOther deriving Eq

lrcoeKindAnd :: LRCoeKind -> LRCoeKind -> LRCoeKind
lrcoeKindAnd LRCoeId LRCoeId = LRCoeId
lrcoeKindAnd _       _       = LRCoeOther

lrcoeKindOfCoe :: Coe -> LRCoeKind
lrcoeKindOfCoe c = if acoreCoeIsId c then LRCoeId else LRCoeOther
%%]

%%[(8 codegen tycore) hs export(LRCoe(..),emptyLRCoe)
data LRCoe
  = LRCoe
      { lrcoeKind		:: LRCoeKind
      , lrcoeLeftL		:: [Coe]
      , lrcoeRightL 	:: [Coe]
      }

emptyLRCoe :: LRCoe
emptyLRCoe = LRCoe LRCoeId [] []
%%]

%%[(8 codegen tycore) hs export(lrcoeIsId)
lrcoeIsId :: LRCoe -> Bool
lrcoeIsId c = lrcoeKind c == LRCoeId
%%]

%%[(9 codegen tycore) hs export(mkLRCoe)
mkLRCoe :: Coe -> Coe -> LRCoe
mkLRCoe l r = LRCoe LRCoeOther [l] [r]

mkIdLRCoe' :: Coe -> Coe -> LRCoe
mkIdLRCoe' l r = LRCoe LRCoeId [l] [r]
%%]

%%[(9 codegen tycore) hs export(mkIdLRCoeWith)
mkIdLRCoeWith :: HsName -> MetaVal -> Ty -> LRCoe
mkIdLRCoeWith n m t = mkIdLRCoe' (acoreCoeAppMeta [(n,m)]) (acoreCoeLam1MetaTy n m t)
%%]

%%[(9 codegen tycore) hs export(lrcoeLSingleton,lrcoeRSingleton,lrcoeLFromList,lrcoeRFromList)
lrcoeLFromList :: [Coe] -> LRCoe
lrcoeLFromList c = LRCoe LRCoeOther c []

lrcoeRFromList :: [Coe] -> LRCoe
lrcoeRFromList c = LRCoe LRCoeOther [] c

lrcoeLSingleton :: Coe -> LRCoe
lrcoeLSingleton c = LRCoe (lrcoeKindOfCoe c) [c] []

lrcoeRSingleton :: Coe -> LRCoe
lrcoeRSingleton c = LRCoe (lrcoeKindOfCoe c) [] [c]
%%]

%%[(8 codegen tycore) hs export(lrcoeUnion)
lrcoeUnion :: LRCoe -> LRCoe -> LRCoe
lrcoeUnion (LRCoe k1 l1 r1) (LRCoe k2 l2 r2) = LRCoe (lrcoeKindAnd k1 k2) (l1 ++ l2) (r1 ++ r2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion construction for implicits
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) hs export(coeImplsAppLVarMp)
coeImplsAppLVarMp :: EHCOpts -> VarMp -> Coe -> [Coe]
coeImplsAppLVarMp opts c coe
  =  case coe of
%%[[9
       Coe_ImplApp i  -> maybe [coe] (acoreCoeImplsApp) (varmpImplsLookupCyc i c)
%%]]
       _              -> [coe]
%%]

%%[(8 codegen tycore) hs export(coeImplsAppRVarMp)
coeImplsAppRVarMp :: VarMp -> Coe -> [Coe]
coeImplsAppRVarMp c coe
  =  case coe of
%%[[9
       Coe_ImplLam i  -> maybe [coe] (acoreCoeImplsLam acoreCoeId) (varmpImplsLookupCyc i c)
%%]]
       _             -> [coe]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion patching
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen tycore) hs export(mkLamBodyCoe)
-- adapt the last coercion of a list of coercions with additional an coercion 'onLast'
mkLamBodyCoe :: Coe -> [Coe] -> [Coe]
mkLamBodyCoe onLast l
  =  case l of
       (_:_)              -> h ++ [onLast `acoreCoeCompose` t]
                          where h = init l
                                t = last l
       _ | acoreCoeIsId onLast -> []
         | otherwise           -> [onLast]
%%]

