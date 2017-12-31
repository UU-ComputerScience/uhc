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

%%[(8 counting) hs import({%{EH}Base.HsName (hsnQualified)})
%%]

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
  pp (Constraint_Gen n t u d n0 d0 c e s) = n >|< ": gen" 
    >|< ppParensCommas [t >#< "^" >#< ppParensCommas [u, d], pp $ "cs: " ++ show c, pp $ "e: " ++ show (M.size e)]
    >#< "==" >#< s >#< "^" >#< ppParensCommas [n0, d0]
    >-< indent 4 (ppMap e)

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

instance PPAnnFree Module where
  ppAnnFree (Module_Module mod _) = ppAnnFree mod

instance PPAnnFree Expr where
  ppAnnFree (Expr_VarLocal v _) = pp v
  ppAnnFree (Expr_VarImport v _) = pp v
  ppAnnFree (Expr_Const c) = pp c
  ppAnnFree (Expr_Abs bind body _) = "\\" >|< bind >#< "->" >-< indent 2 (ppAnnFree body)
  ppAnnFree (Expr_AppLocal func arg _) = ppAnnFree func >#< arg
  ppAnnFree (Expr_AppImport func arg _) = ppAnnFree func >#< arg
  ppAnnFree (Expr_AppConst func arg _) = ppAnnFree func >#< arg
  ppAnnFree (Expr_Let binds body _) = "let" >-< indent 2 (vlist $ map ppAnnFree binds) >-< "in" >#< ppAnnFree body
  ppAnnFree (Expr_LetBang x e1 e2 _) = "let!" >#< x >#< "=" >#< ppAnnFree e1 >-< "in" >#< ppAnnFree e2
  ppAnnFree (Expr_Con _ conNm flds _) = conNm >#< ppSpaces flds 
  ppAnnFree (Expr_Tup flds _) = ppParensCommas flds
  ppAnnFree (Expr_CaseCon e alts _) = "case" >#< ppAnnFree e >#< "of" >-< indent 2 (vlist $ map ppAnnFree alts)
  ppAnnFree (Expr_CaseTup e xs e1 _) = "case" >#< ppAnnFree e >#< "of" >-< indent 2 (ppParensCommas xs >#< "->" >#< ppAnnFree e1)
  ppAnnFree (Expr_CaseConst e alts _) = "case" >#< ppAnnFree e >#< "of" >-< indent 2 (vlist $ map ppAnnFree alts)
  ppAnnFree Expr_FFI{} = text "FFI"
  ppAnnFree (Expr_Ann _ e) = ppAnnFree e
  ppAnnFree (Expr_AnnCore _ e) = ppAnnFree e

instance PPAnnFree Binding where
  ppAnnFree (Binding_Bind n e _) = n >#< "=" >#< ppAnnFree e

instance PPAnnFree AltCon where
  ppAnnFree (AltCon_Alt _ c xs e _) = c >#< ppSpaces xs >#< "->" >#< ppAnnFree e

instance PPAnnFree AltConst where
  ppAnnFree (AltConst_Int c e _) = c >#< "->" >#< ppAnnFree e
  ppAnnFree (AltConst_Char c e _) = c >#< "->" >#< ppAnnFree e

class PPLatex a where
  ppLatex :: a -> PP_Doc

instance PPLatex Module where
  ppLatex (Module_Module mod _) = ppLatex mod

instance PPLatex Expr where
  ppLatex (Expr_VarLocal v _) = ppLatex v
  ppLatex (Expr_VarImport v _) = ppLatex v
  ppLatex (Expr_Const c) = ppLatex c
  ppLatex (Expr_Abs bind body _) = ppParens $ "\\" >|< ppLatex bind >#< "->" >-< indent 2 (ppLatex body)
  ppLatex (Expr_AppLocal func arg _) =  ppParens $ ppLatex func >#< ppLatex arg
  ppLatex (Expr_AppImport func arg _) = ppParens $ ppLatex func >#< ppLatex arg
  ppLatex (Expr_AppConst func arg _) = ppParens $ ppLatex func >#< ppLatex arg
  ppLatex (Expr_Let binds body _) = "let" >-< indent 2 (vlist $ map ppLatex binds) >-< "in" >-< indent 2 (ppLatex body)
  ppLatex (Expr_LetBang x e1 e2 _) = "let!" >-< indent 2 (ppLatex x) >#< "=" >#< ppLatex e1 >-< "in" >-< indent 2 (ppLatex e2)
  ppLatex (Expr_Con _ conNm [] _) = pp conNm
  ppLatex (Expr_Con _ conNm flds _) = ppParens $ conNm >#< ppSpaces (map ppLatex flds) 
  ppLatex (Expr_Tup flds _) = ppParensCommas (map ppLatex flds)
  ppLatex (Expr_CaseCon e alts _) = ppParens $ "case" >#< ppLatex e >#< "of" >-< indent 2 (vlist $ map ppLatex alts)
  ppLatex (Expr_CaseTup e xs e1 _) = ppParens $ "case" >#< ppLatex e >#< "of" >-< indent 2 (ppParensCommas (map ppLatex xs) >#< "->" >#< ppLatex e1)
  ppLatex (Expr_CaseConst e alts _) = ppParens $ "case" >#< ppLatex e >#< "of" >-< indent 2 (vlist (map ppLatex alts))
  ppLatex Expr_FFI{} = text "FFI"
  ppLatex (Expr_Ann t e@(Expr_Let{})) = ppLatex e
  ppLatex (Expr_Ann t e@(Expr_LetBang{})) = ppLatex e
  ppLatex (Expr_Ann t e) = ppLatex e >#< "::" >#< ppLatex t
  ppLatex (Expr_AnnCore _ e) = ppLatex e

instance PPLatex ConVar where
  ppLatex (ConVar_VarLocal v) = ppLatex v
  ppLatex (ConVar_VarImport v) = ppLatex v
  ppLatex (ConVar_Const c) = ppLatex c

instance PPLatex Binding where
  ppLatex (Binding_Bind n e _) = ppLatex n >#< "=" >#< ppLatex e

instance PPLatex Const where
  ppLatex (Const_Int c) = pp c
  ppLatex (Const_Char c) = pp $ show c
  ppLatex (Const_String c) = pp $ show c
  ppLatex (Const_Integer c) = pp c

instance PPLatex AltCon where
  ppLatex (AltCon_Alt _ c [] e _) = ppLatex c >#< "->" >#< ppLatex e
  ppLatex (AltCon_Alt _ c xs e _) = ppLatex c >#< ppSpaces (map ppLatex xs) >#< "->" >#< ppLatex e

instance PPLatex AltConst where
  ppLatex (AltConst_Int c e _) = c >#< "->" >#< ppLatex e
  ppLatex (AltConst_Char c e _) = c >#< "->" >#< ppLatex e
  
instance PPLatex Scheme where
  ppLatex (Scheme_Var v) = ppLatex v
  ppLatex (Scheme_Forall as ts c t) = "forall" >#<
    ppCurlysCommas (map ppLatex $ S.toList as ++ S.toList ts) 
    >#< "." >#< text ("cs: " ++ show (length c))
    >#< "=>"  >#< ppLatex t >-< indent 4 (vlist $ map ppLatex c)
  ppLatex (Scheme_ForallTemp as ts c t) = "forall" >#<
    ppCurlysCommas (map ppLatex $ S.toList as ++ S.toList ts) 
    >#< "." >#< text ("cs: " ++ show (S.size c))
    >#< "=>"  >#< ppLatex t >-< indent 4 (ppCommas $ S.toList c)

instance PPLatex Type where
  ppLatex (Type_Var v) = ppLatex v
  ppLatex (Type_Data n [] []) = ppLatex n
  ppLatex (Type_Data n [] ts) = ppParens $ ppLatex n >#< ppParens (ppSpaces $ map ppLatex ts)
  ppLatex (Type_Data n as []) = ppParens $ ppLatex n >#< ppParens (ppSpaces $ map ppLatex as)
  ppLatex (Type_Data n as ts) = ppParens $ ppLatex n >#< ppParens (ppSpaces $ map ppLatex as) >#< ppParens (ppSpaces $ map ppLatex ts)
  ppLatex (Type_App t1 t2) = ppParens $ ppLatex t1 >#< ppLatex t2
  ppLatex (Type_Func t1 t2) = ppParens (ppLatex t1 >#< "->" >#< ppLatex t2)
  ppLatex (Type_Tup ts) = ppParensCommas $ map ppLatex ts
  ppLatex (Type_Error e) = "Type Error:" >#< e

instance PPLatex RhoType where
  ppLatex (RhoType_Rho (EtaType_Eta t u) d) = "{_u " >|< ppLatex t >#< ppParensCommas (map ppLatex [u,d]) >|< "}"

instance PPLatex EtaType where
  ppLatex (EtaType_Eta t u) = "{_u " >|< ppLatex t >#< ppLatex u >|< "}"

instance PPLatex RhoScheme where
  ppLatex (RhoScheme_Rho (EtaScheme_Eta s u) d) = "{_u " >|< ppParens (ppLatex s) >#< ppParensCommas (map ppLatex [u,d]) >|< "}"

instance PPLatex EtaScheme where
  ppLatex (EtaScheme_Eta s u) = "{_u " >|< ppParens (ppLatex s) >#< ppLatex u >|< "}"

instance PPLatex Annotation where
  ppLatex (Annotation_Var v) = ppLatex v
  ppLatex (Annotation_Val v) = case S.toList v of
    [AnnPrim_Zero] -> text "lzero"
    [AnnPrim_One] -> text "lone"
    [AnnPrim_Infinity] -> text "lomega"
    [AnnPrim_Zero, AnnPrim_One, AnnPrim_Infinity] -> text "top"
    xs -> "{set" >#< (ppCurlysCommas $ map ppLatex xs) >|< "}"
  
instance PPLatex AnnPrim where
  ppLatex AnnPrim_Zero = text "0"
  ppLatex AnnPrim_One = text "1"
  ppLatex AnnPrim_Infinity = text "infty"

instance PPLatex Constraint where
  ppLatex (Constraint_Ann c) = ppLatex c
  ppLatex (Constraint_Eq c) = ppLatex c
  ppLatex (Constraint_Inst n s t) = ppLatex n >|< ": inst(" >|< ppLatex s >|< ") ==" >#< ppLatex t
  ppLatex (Constraint_Gen n t u d n0 d0 c e s) = n >|< ": gen" 
    >|< ppParensCommas [ppLatex t >#< "^" >#< ppParensCommas [ppLatex u, ppLatex d], pp $ "cs: " ++ show c, pp $ "e: " ++ show (M.size e)]
    >#< "==" >#< ppLatex s >#< "^" >#< ppParensCommas [ppLatex n0, ppLatex d0]
    >-< indent 4 (ppMapLatex e)

instance {-# OVERLAPPING #-} PPLatex (Constraints, Map Var Constraints) where
  ppLatex (cs, cm) = vlist $ map ppLatex $ map (,cm) cs

instance {-# OVERLAPPING #-} PPLatex (Constraint, Map Var Constraints) where
  ppLatex (Constraint_Gen n t u d n0 d0 c e s, cm) = ppLatex n >|< ": gen" 
    >|< ppParensCommas [ppLatex t >#< "^" >#< ppParensCommas [ppLatex u, ppLatex d], pp $ "cs: " ++ show (length cs), pp $ "e: " ++ show (M.size e)]
    >#< "==" >#< ppLatex s >#< "^" >#< ppParensCommas [ppLatex n0, ppLatex d0]
    >-< indent 4 (ppLatex (cs, cm)) >-< indent 4 (ppMapLatex e)
    where cs = cm M.! c
  ppLatex (c, _) = ppLatex c

instance PPLatex ConstraintAnn where
  ppLatex (ConstraintAnn_Plus a1 a2 a) = ppLatex a1 >#< "(+)" >#< ppLatex a2 >#< "==" >#< ppLatex a
  ppLatex (ConstraintAnn_Union a1 a2 a) = ppLatex a1 >#< "U" >#< ppLatex a2 >#< "==" >#< ppLatex a
  ppLatex (ConstraintAnn_Times a1 a2 a) = ppLatex a1 >#< "." >#< ppLatex a2 >#< "==" >#< ppLatex a
  ppLatex (ConstraintAnn_Cond a1 a2 a) = ppLatex a1 >#< ">" >#< ppLatex a2 >#< "==" >#< ppLatex a

instance PPLatex ConstraintEq where
  ppLatex (ConstraintEq_Ann a1 a2) = ppLatex a1 >#< "==" >#< ppLatex a2
  ppLatex (ConstraintEq_Type t1 t2) = ppLatex t1 >#< "==" >#< ppLatex t2
  ppLatex (ConstraintEq_Scheme s1 s2) = ppLatex s1 >#< "==" >#< ppLatex s2

ppMapLatex :: (PPLatex k, PPLatex v) => Map k v -> PP_Doc
ppMapLatex = vlist . map f . M.toList
  where f (k,v) = ppLatex k >#< "->" >-< indent 2 (ppLatex v)

instance PPLatex Solution where
  ppLatex (Solution as ts ss) = "annSol:" >-< indent 2 (ppMapLatex as)
    >-< "typeSol:" >-< indent 2 (ppMapLatex ts)
    >-< "schemeSol:" >-< indent 2 (ppMapLatex ss)

-- instance (PPLatex a, PPLatex b) => PPLatex (a,b) where
--   ppLatex (a,b) = pp (ppLatex a, ppLatex b)

-- instance (PPLatex a, PPLatex b, PPLatex c) => PPLatex (a,b,c) where
--   ppLatex (a,b,c) = pp (ppLatex a, ppLatex b, ppLatex c)

-- instance (PPLatex a, PPLatex b, PPLatex c, PPLatex d) => PPLatex (a,b,c,d) where
--   ppLatex (a,b,c,d) = pp (ppLatex a, ppLatex b, ppLatex c, ppLatex d)

instance PPLatex (HsName, Type) where
  ppLatex (n, t) = ppLatex n >#< "->" >-< indent 2 (ppLatex t)

instance PPLatex (HsName, [HsName], [HsName], Map HsName Fields) where
  ppLatex (n, as, ts, flds) = ppSpaces (map ppLatex $ n:as++ts) >-< indent 2 (vlist $ map ppLatex $ M.toList flds)

instance PPLatex Field where
  ppLatex (Field_Strict t) = "!" >|< ppLatex t
  ppLatex (Field_Lazy t) = ppLatex t

instance PPLatex Fields where
  ppLatex flds = ppSpaces $ map ppLatex flds

instance PPLatex (HsName, Fields) where
  ppLatex (n, flds) = n >#< ppLatex flds

instance PPLatex HsName where
  ppLatex = pp . hsnQualified

class PPLatexAnnFree a where
  ppLatexAnnFree :: a -> PP_Doc

ppMapLatexAnnFree :: (PPLatex k, PPLatexAnnFree v) => Map k v -> PP_Doc
ppMapLatexAnnFree = vlist . map f . M.toList
  where f (k,v) = ppLatex k >#< "->" >-< indent 2 (ppLatexAnnFree v)

instance PPLatexAnnFree RhoScheme where
  ppLatexAnnFree (RhoScheme_Rho (EtaScheme_Eta s _) _) = ppLatexAnnFree s
  
instance PPLatexAnnFree Scheme where
  ppLatexAnnFree (Scheme_Var v) = ppLatex v
  ppLatexAnnFree (Scheme_Forall _ ts _ t) | S.null ts = ppLatexAnnFree t
  ppLatexAnnFree (Scheme_Forall _ ts _ t) = "forall" >#< map ppLatex (S.toList ts) >|< "." >#< ppLatexAnnFree t
  ppLatexAnnFree (Scheme_ForallTemp _ ts _ t) | S.null ts = ppLatexAnnFree t
  ppLatexAnnFree (Scheme_ForallTemp _ ts _ t) = "forall" >#< map ppLatex (S.toList ts) >|< "." >#< ppLatexAnnFree t

instance PPLatexAnnFree Type where
  ppLatexAnnFree (Type_Var v) = ppLatex v
  ppLatexAnnFree (Type_Data n _ ts) = ppLatex n >#< ppSpaces (map ppLatexAnnFree ts)
  ppLatexAnnFree (Type_App t1 t2) = ppLatexAnnFree t1 >#< ppLatexAnnFree t2
  ppLatexAnnFree (Type_Func t1 t2) = ppParens (ppLatexAnnFree t1 >#< "->" >#< ppLatexAnnFree t2)
  ppLatexAnnFree (Type_Tup ts) = ppParensCommas (map ppLatexAnnFree ts)
  ppLatexAnnFree (Type_Error e) = "Type Error:" >#< e

instance PPLatexAnnFree RhoType where
  ppLatexAnnFree (RhoType_Rho (EtaType_Eta t _) _) = ppLatexAnnFree t

instance PPLatexAnnFree EtaType where
  ppLatexAnnFree (EtaType_Eta t _) = ppLatexAnnFree t

instance PPLatexAnnFree Constraint where
  ppLatexAnnFree (Constraint_Ann c) = empty
  ppLatexAnnFree (Constraint_Eq c) = ppLatexAnnFree c
  ppLatexAnnFree (Constraint_Inst n s t) = ppLatex n >|< ": inst(" >|< ppLatexAnnFree s >|< ") ==" >#< ppLatexAnnFree t
  ppLatexAnnFree (Constraint_Gen n t u d n0 d0 c e s) = error "Cannot printLatexAnnFree Gen constraint without conMap"

instance {-# OVERLAPPING #-} PPLatexAnnFree (Constraints, Map Var Constraints) where
  ppLatexAnnFree (cs, cm) = vlist $ map (ppLatexAnnFree . (,cm)) cs

instance {-# OVERLAPPING #-} PPLatexAnnFree (Constraint, Map Var Constraints) where
  ppLatexAnnFree (Constraint_Gen n t u d n0 d0 c e s, cm) = ppLatex n >|< ": gen" 
    >|< ppParensCommas [ppLatexAnnFree t , text $ "cs: " ++ show (length cs), text $ "e: " ++ show (M.size e)]
    >#< "==" >#< ppLatex s
    >-< indent 4 (ppLatexAnnFree (cs, cm)) >-< indent 4 (ppMapLatexAnnFree e)
    where cs = cm M.! c
  ppLatexAnnFree (c, cm) = ppLatexAnnFree c 

instance PPLatexAnnFree ConstraintEq where
  ppLatexAnnFree (ConstraintEq_Ann a1 a2) = empty
  ppLatexAnnFree (ConstraintEq_Type t1 t2) = ppLatexAnnFree t1 >#< "==" >#< ppLatexAnnFree t2
  ppLatexAnnFree (ConstraintEq_Scheme s1 s2) = ppLatexAnnFree s1 >#< "==" >#< ppLatexAnnFree s2

instance PPLatexAnnFree Solution where
  ppLatexAnnFree (Solution _ ts ss) = "typeSol:" >-< indent 2 (ppMapLatexAnnFree ts)
    >-< "schemeSol:" >-< indent 2 (ppMapLatexAnnFree ss)

instance PPLatexAnnFree Module where
  ppLatexAnnFree (Module_Module mod _) = ppLatexAnnFree mod

instance PPLatexAnnFree Expr where
  ppLatexAnnFree (Expr_VarLocal v _) = ppLatex v
  ppLatexAnnFree (Expr_VarImport v _) = ppLatex v
  ppLatexAnnFree (Expr_Const c) = ppLatex c
  ppLatexAnnFree (Expr_Abs bind body _) = "\\" >|< ppLatex bind >#< "->" >-< indent 2 (ppLatexAnnFree body)
  ppLatexAnnFree (Expr_AppLocal func arg _) = ppLatexAnnFree func >#< ppLatex arg
  ppLatexAnnFree (Expr_AppImport func arg _) = ppLatexAnnFree func >#< ppLatex arg
  ppLatexAnnFree (Expr_AppConst func arg _) = ppLatexAnnFree func >#< ppLatex arg
  ppLatexAnnFree (Expr_Let binds body _) = "let" >-< indent 2 (vlist $ map ppLatexAnnFree binds) >-< "in" >#< ppLatexAnnFree body
  ppLatexAnnFree (Expr_LetBang x e1 e2 _) = "let!" >#< ppLatex x >#< "=" >#< ppLatexAnnFree e1 >-< "in" >#< ppLatexAnnFree e2
  ppLatexAnnFree (Expr_Con _ conNm flds _) = ppLatex conNm >#< ppSpaces flds 
  ppLatexAnnFree (Expr_Tup flds _) = ppParensCommas flds
  ppLatexAnnFree (Expr_CaseCon e alts _) = "case" >#< ppLatexAnnFree e >#< "of" >-< indent 2 (vlist $ map ppLatexAnnFree alts)
  ppLatexAnnFree (Expr_CaseTup e xs e1 _) = "case" >#< ppLatexAnnFree e >#< "of" >-< indent 2 (ppParensCommas (map ppLatex xs) >#< "->" >#< ppLatexAnnFree e1)
  ppLatexAnnFree (Expr_CaseConst e alts _) = "case" >#< ppLatexAnnFree e >#< "of" >-< indent 2 (vlist $ map ppLatexAnnFree alts)
  ppLatexAnnFree Expr_FFI{} = text "FFI"
  ppLatexAnnFree (Expr_Ann _ e) = ppLatexAnnFree e
  ppLatexAnnFree (Expr_AnnCore _ e) = ppLatexAnnFree e

instance PPLatexAnnFree Binding where
  ppLatexAnnFree (Binding_Bind n e _) = ppLatex n >#< "=" >#< ppLatexAnnFree e

instance PPLatexAnnFree AltCon where
  ppLatexAnnFree (AltCon_Alt _ c xs e _) = ppLatex c >#< ppSpaces (map ppLatex xs) >#< "->" >#< ppLatexAnnFree e

instance PPLatexAnnFree AltConst where
  ppLatexAnnFree (AltConst_Int c e _) = c >#< "->" >#< ppLatexAnnFree e
  ppLatexAnnFree (AltConst_Char c e _) = c >#< "->" >#< ppLatexAnnFree e

%%]
