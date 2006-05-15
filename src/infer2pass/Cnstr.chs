%%[1 import(Data.List,Common,Ty) export(Cnstr,emptyCnstr,cnstrPlus,cnstrTyLookup)
%%]

%%[3 export(cnstrTyUnit,cnstrTyRevUnit,cnstrKeys,cnstrDel)
%%]

%%[3 import(Data.Maybe) export(cnstrDelAlphaRename,cnstrFilterAlphaRename,cnstrFilterTyAltsMappedBy)
%%]

-------------------------------------------------------------------------
-- Cnstr
-------------------------------------------------------------------------

%%[1
type Cnstr = [(TyVarId,Ty)]

emptyCnstr :: Cnstr
emptyCnstr = []

cnstrPlus :: Cnstr -> Cnstr -> Cnstr
cnstrPlus s1 s2 = s1 ++ deleteFirstsBy (\(v1,_) (v2,_) -> v1 == v2) s2 s1
%%]

%%[1
cnstrTyLookup :: TyVarId -> Cnstr -> Maybe Ty
cnstrTyLookup tv s = lookup tv s
%%]

%%[3
cnstrTyUnit :: TyVarId -> Ty -> Cnstr
cnstrTyUnit tv t = [(tv,t)]

cnstrTyRevUnit :: TyVarId -> Ty -> (Ty,Cnstr)
cnstrTyRevUnit tv t = maybe (t,cnstrTyUnit tv t) (\v -> let t' = mkTyVar tv in (t',cnstrTyUnit v t')) . tyMbVar $ t

cnstrDel :: TyVarIdL -> Cnstr -> Cnstr
cnstrDel tvL c = cnstrFilterTy (const.not.(`elem` tvL)) c
%%]

%%[3
cnstrFilterTy :: (TyVarId -> Ty -> Bool) -> Cnstr -> Cnstr
cnstrFilterTy f = filter (uncurry f)
%%]

%%[3
cnstrKeys :: Cnstr -> TyVarIdL
cnstrKeys = map fst
%%]

%%[3
cnstrDelAlphaRename :: Cnstr -> Cnstr
cnstrDelAlphaRename = cnstrFilterTy (\_ t -> not (tyIsVar t))

cnstrFilterAlphaRename :: Cnstr -> Cnstr
cnstrFilterAlphaRename = cnstrFilterTy (\_ t -> case t of {Ty_Var _ -> True ; _ -> False})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Filter cnstr bound to Ty_Alts which has a cnstr in other
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[3
cnstrFilterTyAltsMappedBy :: Cnstr -> Cnstr -> Cnstr
cnstrFilterTyAltsMappedBy c cMp
  =  cnstrFilterTy (\_ t -> case t of {Ty_Alts v _ -> isJust (cnstrTyLookup v cMp) ; _ -> False}) c
%%]
