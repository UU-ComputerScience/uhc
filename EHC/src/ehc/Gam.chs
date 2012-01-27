%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma (aka Assumptions, Environment)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam} import({%{EH}Gam.Base}) export(module {%{EH}Gam.Base})
%%]

%%[1 module {%{EH}Gam} import(Data.List,EH.Util.Utils,{%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}NameAspect})
%%]

%%[(1 hmtyinfer || hmtyast).Ty import({%{EH}Ty})
%%]

%%[4 -1.Ty import({%{EH}Ty})
%%]

%%[1 import({%{EH}Error}) 
%%]

%%[1 import(EH.Util.Pretty) export(ppGam,ppGamDup)
%%]

%%[(1 hmtyinfer || hmtyast) import({%{EH}Ty.Pretty})
%%]

%%[1 export(IdDefOccGam,IdDefOccAsc)
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(2 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(3333 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.Quantify})
%%]

%%[4 import({%{EH}Opts.Base})
%%]

%%[(4444 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.Instantiate})
%%]

%%[(4444 hmtyinfer) import({%{EH}Ty.FitsInCommon})
%%]

%%[4_2 export(ErrGam)
%%]

%%[4_2 export(valGamInst1ExistsWithVarMp)
%%]

%%[7 import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[(8 codegen) import({%{EH}Core})
%%]

%%[(9999 hmtyinfer) import({%{EH}Ty.FitsInCommon})
%%]

%%[9.ScopeMapGam import({%{EH}Gam.ScopeMapGam})
%%]

%%[9 import({%{EH}Base.Debug})
%%]

%%[(9 codegen) import({%{EH}Core.Subst})
%%]
%%[(9 hmtyinfer || hmtyast) hs import({%{EH}VarLookup})
%%]

%%[9 export(idDefOccGamPartitionByKind)
%%]

%%[50 export(idDefOccGamByKind)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%[(9999 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Iterating
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 hmtyinfer || hmtyast) export(gamDoTyWithVarMp)
-- Do something with each Ty in a Gam.
-- The global VarMp is kept separately so a new tyvar binding can be computed, which is threaded separatedly and also returned.
-- This allows the retainment of the original tyvar in the Gam, which is required when used twice with different VarMp's.
gamDoTyWithVarMp
  :: Ord key =>
     (info -> Ty,Ty -> info -> info)									-- get/set from/into info in Gam
     																	-- do whatever must be done given
     -> (key															-- 	 name in gam
         -> (Ty,VarMp)													--   Ty + cycles
         -> VarMp														--   new subst
         -> thr															--   thread
         -> (Ty,VarMp,thr))												--   result: new Ty, new subst, thread
     -> VarMp															-- subst for Gam entries
     -> thr																-- initial value for thread
     -> Gam key info													-- the Gam (env)
     -> (Gam key info,VarMp,thr)										-- result: new gam, new subst, thread
gamDoTyWithVarMp (get,set) f gamVarMp thr gam
  = (g,c,thr')
  where (g,(thr',c))
           = gamMapThr
               (\(n,gi) (thr,c)
                   -> let t = get gi
                          (t',c',thr') = f n (gamVarMp `varUpdCyc` t) c thr
                          (tg,cg)      = case (tyUnAnn t,tyUnAnn t') of
                                           (Ty_Var v1 _  ,Ty_Var v2 _) | v1 == v2
                                             -> dflt
                                           (Ty_Var v  cat,_          ) | not (tvCatIsFixed cat)
                                             -> (t ,v `varmpTyUnit` t')
                                           _ -> dflt
                                       where dflt = (t',emptyVarMp)
                      in  ((n,set ({- tr "gamDoTyWithVarMp.set" (ppTy tg) $ -} tg) gi),(thr',{- (\v -> tr "gamDoTyWithVarMp" (pp v) v) $ -} cg `varmpPlus` c'))
               )
               (thr,emptyVarMp) gam
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Error" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2
type ErrGam = Gam HsName ErrL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "XX of sort" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(SoGam, SoGamInfo(..))
data SoGamInfo
  = SoGamInfo
      { sgiSo :: Ty }
      deriving Show

type SoGam = Gam HsName SoGamInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier definition occurrence gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
type IdDefOccGam = Gam    IdOcc  IdDefOcc
type IdDefOccAsc = AssocL IdOcc [IdDefOcc]
%%]

%%[9
idDefOccGamPartitionByKind :: [IdOccKind] -> IdDefOccGam -> (IdDefOccAsc,IdDefOccAsc)
idDefOccGamPartitionByKind ks
  = partition (\(IdOcc n k',_) -> k' `elem` ks) . gamToAssocDupL
%%]

%%[50
idDefOccGamByKind :: IdOccKind -> IdDefOccGam -> AssocL HsName IdDefOcc
idDefOccGamByKind k g = [ (n,head i) | (IdOcc n _,i) <- fst (idDefOccGamPartitionByKind [k] g) ]
%%]

%%[50 export(idDefOccGamStrip)
-- | Strip references to original source file location
idDefOccGamStrip :: IdDefOccGam -> IdDefOccGam
idDefOccGamStrip g = gamMap (\(k,v) -> (k,doccStrip v)) g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier definition additional aspect gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not used

%%[1111 export(IdDefAspGam,IdDefAspAsc)
type IdDefAspGam = Gam    IdOcc  IdDefAsp
type IdDefAspAsc = AssocL IdOcc [IdDefAsp]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier unqualified to qualified gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(IdQualGam)
type IdQualGam = Gam IdOcc HsName
%%]

%%[50 export(idGam2QualGam,idQualGamReplacement)
idGam2QualGam :: IdDefOccGam -> IdQualGam
idGam2QualGam = gamMap (\(iocc,docc) -> (iocc {ioccNm = hsnQualified $ ioccNm iocc},ioccNm $ doccOcc $ docc))

idQualGamReplacement :: IdQualGam -> IdOccKind -> HsName -> HsName
idQualGamReplacement g k n = maybe n id $ gamLookup (IdOcc n k) g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances for Substitutable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).Substitutable.Gam
instance (Eq tk,VarUpdatable vv subst) => VarUpdatable (Gam tk vv) subst where
  s `varUpd`  (Gam ll)    =   Gam (map (assocLMapElt (s `varUpd`)) ll)
%%[[4
  s `varUpdCyc` (Gam ll)    =   (Gam ll',varmpUnions $ map (varmpUnions . assocLElts) m)
                     where (ll',m) = unzip $ map (assocLMapUnzip . assocLMapElt (s `varUpdCyc`)) ll
%%]]

instance (Eq k,Eq tk,VarExtractable vv k) => VarExtractable (Gam tk vv) k where
  varFreeSet (Gam ll)    =   Set.unions . map varFreeSet . map snd . concat $ ll
%%]

%%[(9 hmtyinfer || hmtyast).Substitutable.SGam -2.Substitutable.Gam
instance (Ord tk,VarUpdatable vv subst) => VarUpdatable (SGam tk vv) subst where
  s `varUpd`  g    =   gamMapElts (s `varUpd`) g
%%[[4
  s `varUpdCyc` g    =   (g',varmpUnions $ gamElts gm)
              where (g',gm) = sgamUnzip $ gamMapElts (s `varUpdCyc`) g
%%]]

instance (Ord tk,Ord k,VarExtractable vv k) => VarExtractable (SGam tk vv) k where
  varFreeSet g    =   Set.unions $ map varFreeSet $ gamElts g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ppGam
ppGam :: (Ord k, PP k, PP v) => Gam k v -> PP_Doc
ppGam g = ppAssocL (gamToAssocL g)
%%]

%%[1
ppGamDup :: (Ord k,PP k, PP v) => Gam k v -> PP_Doc
ppGamDup g = ppAssocL $ map (\(k,v) -> (k,ppBracketsCommas v)) $ gamToAssocDupL $ g
%%]

%%[1.PP.Gam
instance (Ord k, PP k, PP v) => PP (Gam k v) where
  pp = ppGam
%%]

%%[9.PP.Gam -1.PP.Gam
instance (Ord k, PP k, PP v) => PP (SGam k v) where
  pp g = ppGam g
%%]

%%[9999 -(9.PP.Gam 1.PP.Gam)
instance (Ord k, PP k, PP v) => PP (SGam k v) where
  pp g = ppGam g
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 hmtyinfer || hmtyast)
instance ForceEval TyKiKey
%%[[102
  where
    fevCount (TyKiKey_Name  n) = cm1 "TyKiKey_Name"  `cmUnion` fevCount n
    fevCount (TyKiKey_TyVar v) = cm1 "TyKiKey_TyVar" `cmUnion` fevCount v
%%]]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of soGam, only used by TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(initSoGam)
initSoGam :: SoGam
initSoGam
  = assocLToGam
      [ (hsnKindStar,   SoGamInfo kiStar)
      ]
%%]


