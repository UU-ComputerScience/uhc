%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substituting holes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}TyCore.Subst} import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map,EH.Util.Pretty,EH.Util.Utils,{%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}VarMp})
%%]

%%[(8 codegen) hs import({%{EH}TyCore.Base},{%{EH}TyCore.Pretty},{%{EH}TyCore.Coercion})
%%]

%%[(8 codegen) hs import(qualified {%{EH}Ty} as T)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CSubstKey(..),CSubstInfo(..),CSubst,emptyCSubst)
data CSubstKey
  = CSKey_UID   UID
  | CSKey_Nm    HsName
  deriving (Show,Eq,Ord)

data CSubstInfo
    =  CSITy        {csiTy      :: !Ty                                           }
    |  CSIExpr      {csiRepl    :: !Expr                                           }
%%[[9
    |  CSIImpls     {csiAppCoeL :: ![Coe]       , csiLamCoeL    :: ![Coe]           }
    |  CSIBinds     {csiBindL   :: !ValBindL                                          }
%%]]
    deriving Show

type CSubst = Map.Map CSubstKey CSubstInfo

emptyCSubst :: CSubst
emptyCSubst = Map.empty
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code substitution as class
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(CSubstitutable(..))
infixr `cSubstApp`

class CSubstitutable a where
  cSubstApp :: CSubst -> a -> a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(uidTyLToCSubst)
uidTyLToCSubst :: AssocL HsName Ty -> CSubst
uidTyLToCSubst l = Map.fromList [ (CSKey_Nm k,CSITy v) | (k,v) <- l ]
-- uidTyLToCSubst = Map.fromList . assocLMapElt CSITy
%%]
  
%%[(8 codegen) hs export(uidExprLToCSubst)
uidExprLToCSubst :: AssocL UID Expr -> CSubst
uidExprLToCSubst l = Map.fromList [ (CSKey_UID k,CSIExpr v) | (k,v) <- l ]
-- uidExprLToCSubst = Map.fromList . assocLMapElt CSIExpr

%%]
  
%%[(9 codegen) hs export(uidImplsLToCSubst,uidValBindLLToCSubst,poiExprLToCSubst,cnstrImplsToCSubst)
uidValBindLLToCSubst :: AssocL UID ValBindL -> CSubst
uidValBindLLToCSubst l = Map.fromList [ (CSKey_UID k,CSIBinds v) | (k,v) <- l ]
-- uidValBindLLToCSubst = Map.fromList . assocLMapElt CSIBinds

poiExprLToCSubst :: AssocL PredOccId Expr -> CSubst
poiExprLToCSubst = uidExprLToCSubst . assocLMapKey poiId

uidImplsLToCSubst :: AssocL UID ([Coe],[Coe]) -> CSubst
uidImplsLToCSubst l = Map.fromList [ (CSKey_UID k,uncurry CSIImpls v) | (k,v) <- l ]
-- uidImplsLToCSubst = Map.fromList . assocLMapElt (uncurry CSIImpls)

cnstrImplsToCSubst :: EHCOpts -> VarMp -> CSubst
cnstrImplsToCSubst opts c
  =  uidImplsLToCSubst
        [ (iv,(mkImplsAppCoe opts i,mkImplsLamCoe coeId i))
        | (iv,VMIImpls i) <- varmpToAssocL c, let (_,mbTl) = T.implsPredsMbTail i, isNothing mbTl
        ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CSubst combination
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

On CSubst, merges only, application is postponed

%%[(8 codegen) hs export(cSubstAppSubst)
cSubstAppSubst :: CSubst -> CSubst -> CSubst
cSubstAppSubst = Map.union
%%]
  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Coercion: canceling (wiping) & combining (weaving)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LRCoe coercion for lamda
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) hs export(lrcoeWipeWeaveAsSubst)
lrcoeWipeWeaveAsSubst :: EHCOpts -> UID -> VarMp -> LRCoe -> (Coe,CSubst)
lrcoeWipeWeaveAsSubst opts uniq cnstr (LRCoe LRCoeId _ _) = (coeId,emptyCSubst)
lrcoeWipeWeaveAsSubst opts uniq cnstr lrcoe               = coeWipeWeaveAsSubst opts uniq cnstr (lrcoeLeftL lrcoe) (lrcoeRightL lrcoe)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instance on CSubst
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs
instance PP CSubstKey where
  pp (CSKey_UID i)  = pp i
  pp (CSKey_Nm  n)  = pp n
%%]


