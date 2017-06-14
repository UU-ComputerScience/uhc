%%[(8 counting) hs module {%{EH}CountingAnalysis.Pretty}
%%]

%%[(8 counting) hs import(Data.Set (Set),qualified Data.Set as S,Data.Map (Map),qualified Data.Map as M)
%%]

-- %%[(8 counting) hs import({%{EH}Base.HsName (HsName, hsnIsNr)}, {%{EH}Core})
-- %%]

%%[(8 counting) hs import(UHC.Util.Pretty, {%{EH}CountingAnalysis})
%%]

-- %%[(8 counting) hs import(Control.Monad)
-- %%]

-- %%[(8 counting) hs import(qualified Data.Sequence as Seq, qualified Data.Foldable as Fold)
-- %%]

-- %%[(8 counting) hs import({%{EH}Base.Target (FFIWay)}, {%{EH}Foreign (ForeignEnt)}, {%{EH}Ty (Ty)})
-- %%]

-- Pretty printing
%%[(8 counting) hs
instance PP Module where
  pp (Module_Module mod _) = pp mod

instance PP Expr where
  pp (Expr_VarLocal v _) = pp v
  pp (Expr_VarImport v _) = pp v
  pp (Expr_Const c) = pp c
  pp (Expr_Abs bind body _) = "\\" >|< bind >#< "->" >-< indent 2 body
  pp (Expr_AppLocal func arg _) = func >#< arg
  pp (Expr_AppImport func arg _) = func >#< arg
  pp (Expr_AppConst func arg _) = func >#< arg
  pp (Expr_Let binds body _) = "let" >-< indent 2 (vlist binds) >-< "in" >#< body
  pp (Expr_LetBang x e1 e2 _) = "let!" >#< x >#< "=" >#< e1 >-< "in" >#< e2
  pp (Expr_Con _ conNm flds _) = conNm >#< ppSpaces flds 
  pp (Expr_Tup flds _) = ppParensCommas flds
  pp (Expr_CaseCon e alts _) = "case" >#< e >#< "of" >-< indent 2 (vlist alts)
  pp (Expr_CaseTup e xs e1 _) = "case" >#< e >#< "of" >-< indent 2 (ppParensCommas xs >#< "->" >#< e1)
  pp (Expr_CaseConst e alts _) = "case" >#< e >#< "of" >-< indent 2 (vlist alts)
  pp Expr_FFI{} = text "FFI"
  pp (Expr_Ann t e) = ppParens e >#< "::" >#< t
  pp (Expr_AnnCore _ e) = pp e

instance PP ConVar where
  pp (ConVar_VarLocal v) = pp v
  pp (ConVar_VarImport v) = pp v
  pp (ConVar_Const c) = pp c

instance PP Binding where
  pp (Binding_Bind n e _) = n >#< "=" >#< e

instance PP Const where
  pp (Const_Int c) = pp c
  pp (Const_Char c) = pp $ show c
  pp (Const_String c) = pp $ show c
  pp (Const_Integer c) = pp c

instance PP AltCon where
  pp (AltCon_Alt _ c xs e _) = c >#< ppSpaces xs >#< "->" >#< e

instance PP AltConst where
  pp (AltConst_Int c e _) = c >#< "->" >#< e
  pp (AltConst_Char c e _) = c >#< "->" >#< e
  
instance PP Scheme where
  pp (Scheme_Var v) = pp v
  pp (Scheme_Forall as ts c t) = "forall" >#<
    ppCurlysCommas (S.toList as ++ S.toList ts) 
    >#< "." >#< text ("cs: " ++ show (length c))
    >#< "=>"  >#< t >-< indent 4 (vlist c)
  pp (Scheme_ForallTemp as ts c t) = "forall" >#<
    ppCurlysCommas (S.toList as ++ S.toList ts) 
    >#< "." >#< text ("cs: " ++ show (S.size c))
    >#< "=>"  >#< t >-< indent 4 (hlist $ S.toList c)

instance PP Type where
  pp (Type_Var v) = pp v
  pp (Type_Data n as ts) = n >#< ppSpaces as >#< ppSpaces ts
  pp (Type_App t1 t2) = t1 >#< t2
  pp (Type_Func t1 t2) = ppParens (t1 >#< "->" >#< t2)
  pp (Type_Tup ts) = ppParensCommas ts
  pp (Type_Error e) = "Type Error:" >#< e

instance PP RhoType where
  pp (RhoType_Rho (EtaType_Eta t u) d) = ppParens t >#< "^" >#< ppParensCommas [u,d]

instance PP EtaType where
  pp (EtaType_Eta t u) = ppParens t >#< "^" >#< u

instance PP RhoScheme where
  pp (RhoScheme_Rho (EtaScheme_Eta s u) d) = ppParens s >#< "^" >#< ppParensCommas [u,d]

instance PP EtaScheme where
  pp (EtaScheme_Eta s u) = ppParens s >#< "^" >#< u

instance PP Annotation where
  pp (Annotation_Var v) = pp v
  pp (Annotation_Val v) = ppCurlysCommas $ S.toList v
  
instance PP AnnPrim where
  pp AnnPrim_Zero = text "0"
  pp AnnPrim_One = text "1"
  pp AnnPrim_Infinity = text "2"

instance PP Constraint where
  pp (Constraint_Ann c) = pp c
  pp (Constraint_Eq c) = pp c
  pp (Constraint_Inst n s t) = n >|< ": inst(" >|< s >|< ") ==" >#< t
  pp (Constraint_Gen n t u d n0 d0 c e s) = error "Cannot print Gen constraint without ConstraintMap"

instance {-# OVERLAPPING #-} PP (Constraints, Map Var Constraints) where
  pp (cs, cm) = vlist $ map (,cm) cs

instance {-# OVERLAPPING #-} PP (Constraint, Map Var Constraints) where
  pp (Constraint_Gen n t u d n0 d0 c e s, cm) = n >|< ": gen" 
    >|< ppParensCommas [t >#< "^" >#< ppParensCommas [u, d], pp $ "cs: " ++ show (length cs), pp $ "e: " ++ show (M.size e)]
    >#< "==" >#< s >#< "^" >#< ppParensCommas [n0, d0]
    >-< indent 4 (pp (cs, cm)) >-< indent 4 (ppMap e)
    where cs = cm M.! c
  pp (c, _) = pp c

instance PP ConstraintAnn where
  pp (ConstraintAnn_Plus a1 a2 a) = a1 >#< "(+)" >#< a2 >#< "==" >#< a
  pp (ConstraintAnn_Union a1 a2 a) = a1 >#< "U" >#< a2 >#< "==" >#< a
  pp (ConstraintAnn_Times a1 a2 a) = a1 >#< "." >#< a2 >#< "==" >#< a
  pp (ConstraintAnn_Cond a1 a2 a) = a1 >#< ">" >#< a2 >#< "==" >#< a

instance PP ConstraintEq where
  pp (ConstraintEq_Ann a1 a2) = a1 >#< "==" >#< a2
  pp (ConstraintEq_Type t1 t2) = t1 >#< "==" >#< t2
  pp (ConstraintEq_Scheme s1 s2) = s1 >#< "==" >#< s2

ppMap :: (PP k, PP v) => Map k v -> PP_Doc
ppMap = vlist . map f . M.toList
  where f (k,v) = k >#< "->" >-< indent 2 (pp v)

instance PP Solution where
  pp (Solution as ts ss) = "annSol:" >-< indent 2 (ppMap as)
    >-< "typeSol:" >-< indent 2 (ppMap ts)
    >-< "schemeSol:" >-< indent 2 (ppMap ss)

class PPAnnFree a where
  ppAnnFree :: a -> PP_Doc

ppMapAnnFree :: (PP k, PPAnnFree v) => Map k v -> PP_Doc
ppMapAnnFree = vlist . map f . M.toList
  where f (k,v) = k >#< "->" >-< indent 2 (ppAnnFree v)

instance PPAnnFree RhoScheme where
  ppAnnFree (RhoScheme_Rho (EtaScheme_Eta s _) _) = ppAnnFree s
  
instance PPAnnFree Scheme where
  ppAnnFree (Scheme_Var v) = pp v
  ppAnnFree (Scheme_Forall _ ts _ t) = "forall" >#< ts >|< "." >#< ppAnnFree t
  ppAnnFree (Scheme_ForallTemp _ ts _ t) = "forall" >#< ts >|< "." >#< ppAnnFree t

instance PPAnnFree Type where
  ppAnnFree (Type_Var v) = pp v
  ppAnnFree (Type_Data n _ ts) = n >#< ppSpaces (map ppAnnFree ts)
  ppAnnFree (Type_App t1 t2) = ppAnnFree t1 >#< ppAnnFree t2
  ppAnnFree (Type_Func t1 t2) = ppParens (ppAnnFree t1 >#< "->" >#< ppAnnFree t2)
  ppAnnFree (Type_Tup ts) = ppParensCommas (map ppAnnFree ts)
  ppAnnFree (Type_Error e) = "Type Error:" >#< e

instance PPAnnFree RhoType where
  ppAnnFree (RhoType_Rho (EtaType_Eta t _) _) = ppAnnFree t

instance PPAnnFree EtaType where
  ppAnnFree (EtaType_Eta t _) = ppAnnFree t

instance PPAnnFree Constraint where
  ppAnnFree (Constraint_Ann c) = empty
  ppAnnFree (Constraint_Eq c) = ppAnnFree c
  ppAnnFree (Constraint_Inst n s t) = n >|< ": inst(" >|< ppAnnFree s >|< ") ==" >#< ppAnnFree t
  ppAnnFree (Constraint_Gen n t u d n0 d0 c e s) = error "Cannot printAnnFree Gen constraint without conMap"

instance {-# OVERLAPPING #-} PPAnnFree (Constraints, Map Var Constraints) where
  ppAnnFree (cs, cm) = vlist $ map (ppAnnFree . (,cm)) cs

instance {-# OVERLAPPING #-} PPAnnFree (Constraint, Map Var Constraints) where
  ppAnnFree (Constraint_Gen n t u d n0 d0 c e s, cm) = n >|< ": gen" 
    >|< ppParensCommas [ppAnnFree t , text $ "cs: " ++ show (length cs), text $ "e: " ++ show (M.size e)]
    >#< "==" >#< s
    >-< indent 4 (ppAnnFree (cs, cm)) >-< indent 4 (ppMapAnnFree e)
    where cs = cm M.! c
  ppAnnFree (c, cm) = ppAnnFree c 

instance PPAnnFree ConstraintEq where
  ppAnnFree (ConstraintEq_Ann a1 a2) = empty
  ppAnnFree (ConstraintEq_Type t1 t2) = ppAnnFree t1 >#< "==" >#< ppAnnFree t2
  ppAnnFree (ConstraintEq_Scheme s1 s2) = ppAnnFree s1 >#< "==" >#< ppAnnFree s2

instance PPAnnFree Solution where
  ppAnnFree (Solution _ ts ss) = "typeSol:" >-< indent 2 (ppMapAnnFree ts)
    >-< "schemeSol:" >-< indent 2 (ppMapAnnFree ss)
%%]
