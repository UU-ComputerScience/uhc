%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run GRI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module GRINIRun import(EH.Util.Pretty,{%{EH}Base.Common},{%{EH}Base.Builtin},GRINICommon,{%{EH}GrinCode},{%{EH}GrinCode.Pretty})
%%]

%%[8 import(qualified Data.Map as Map,Data.Maybe,Data.Array,Data.Array.IO)
%%]

%%[8 export(ppRunState)
%%]

%%[8 export(RunState(..),RunHeap(..),RunVal(..),RunLoc,RunEnv,run1Step,mkRN,NdCat(..),rvHole)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Machine values
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
data NdCat = NdCon | NdHole | NdRec | NdApp | NdFun | NdPApp deriving (Enum,Eq,Ord,Show)

data RunVal
  =  RVNil
  |  RVCat      !NdCat
  |  RVInt      !Int
  |  RVStr      !String
  |  RVNode     !(Array Int RunVal)
  |  RVPtr      !Int
  |  RVGlob     !HsName ![HsName] !GrExpr

mkRN :: [RunVal] -> RunVal
mkRN es = RVNode (listArray (0,length es - 1) es)

rvHole :: RunVal
rvHole = mkRN [RVCat NdHole]

instance Show RunVal where
  show v = "VAL"

instance PP RunVal where
  pp (RVNil             ) = pp "-:-"
  pp (RVCat     v       ) = "C:" >|< pp (drop 2 (show v))
  pp (RVInt     v       ) = "I:" >|< pp v
  pp (RVStr     v       ) = "S:" >|< pp v
  pp (RVNode    v       ) = "N:" >|< ppListSep "(" ")" " " (elems v)
  pp (RVPtr     v       ) = "P:" >|< pp v
  pp (RVGlob    v ns e  ) = "G:" >|< pp v >#< ppSpaces ns -- >#< "= ..."
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Machine state, memory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type RunLoc = Maybe GrExpr

data RunHeap = RunHeap {rhMem :: !(IOArray Int RunVal), rhSize :: !Int, rhFree :: !Int}

type RunEnv = Map.Map HsName RunVal

type GrPat = Either GrPatLam GrPatAlt

data RunState
  =  RunState
        {   rsNext          ::  !RunLoc
        ,   rsStack         ::  ![(GrPat,GrExpr,RunEnv)]
        ,   rsEnv           ::  !RunEnv
        ,   rsGlobEnv       ::  !RunEnv
        ,   rsHeap          ::  !RunHeap
        ,   rsHalted        ::  Maybe PP_Doc
        ,   rsNrSteps       ::  !Int
        }

rsVar :: RunState -> HsName -> RunVal
rsVar rs n = Map.findWithDefault RVNil n (rsEnv rs)

rsDeref :: RunState -> RunVal -> IO RunVal
rsDeref rs r
  =  case r of
        RVPtr p | p < rhFree h -> readArray (rhMem h) p
                | otherwise    -> return RVNil
              where h = rsHeap rs
        _ -> return r

rsVarDeref :: RunState -> HsName -> IO RunVal
rsVarDeref rs = rsDeref rs . rsVar rs

ppRunHeap :: RunHeap -> IO PP_Doc
ppRunHeap h
  =  do  {  m <- getAssocs . rhMem $ h
         ;  return
                ("HEAP sz=" >|< rhSize h >|< " fr=" >|< rhFree h
                    >-< indent 2 (ppAssocL m))
         }

ppRunState :: RunState -> IO PP_Doc
ppRunState rs
  =  do  {  h <- ppRunHeap (rsHeap rs)
         ;  return
                ("STATE next=" >|< maybe empty ppGrExpr (rsNext rs)
                  >-< indent 2
                        (h
                         >-< "ENV" >#< (ppFM . rsEnv $ rs)
                         >-< "STK" >#< (vlist . map (\(p,rs,_) -> either ppGrPatLam ppGrPatAlt p >#< "->" >#< ppGrExpr rs) . rsStack $ rs)
                        ))
         }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Primitives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
primMp :: Map.Map String (RunState -> [RunVal] -> IO (RunState,Maybe RunVal))
primMp
  =  Map.fromList
        [ (show hsnPrimAddInt
            ,\rs [RVInt i1,RVInt i2]    ->  return (rs,Just (RVInt (i1 + i2)))
          )
        , ("primDivInt"
            ,\rs [RVInt i1,RVInt i2]    ->  return (rs,Just (RVInt (i1 `div` i2)))
          )
        , ("primMulInt"
            ,\rs [RVInt i1,RVInt i2]    ->  return (rs,Just (RVInt (i1 * i2)))
          )
        , ("primSubInt"
            ,\rs [RVInt i1,RVInt i2]    ->  return (rs,Just (RVInt (i1 - i2)))
          )
        , ("primCmpInt"
            ,\rs [RVInt i1,RVInt i2]    ->  let  c = case i1 `compare` i2 of {EQ->0; GT->1; LT->2}
                                            in   return (rs,Just (mkRN [RVCat NdCon,RVInt c,RVInt 0]))
          )
        , ("primEqInt"
            ,\rs [RVInt i1,RVInt i2]    ->  let  c = case i1 == i2 of {False->0; True->1}
                                            in   return (rs,Just (mkRN [RVCat NdCon,RVInt c,RVInt 0]))
          )
        , ("primLtInt"
            ,\rs [RVInt i1,RVInt i2]    ->  let  c = case i1 < i2 of {False->0; True->1}
                                            in   return (rs,Just (mkRN [RVCat NdCon,RVInt c,RVInt 0]))
          )
        , ("primCStringToString"
            ,\rs [v@(RVStr _)]    ->  return (rs,Just v)
          )
        , ("primTraceStringExit"
            ,\rs [RVStr s]        ->  do { rs' <- halt rs (pp s)
                                         ; return (rs',Just RVNil)
                                         }
          )
        , ("primUndefined"
            ,\rs _                ->  do { rs' <- halt rs (pp "undefined")
                                         ; return (rs',Just RVNil)
                                         }
          )
        ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running the machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
grEvalTag :: RunState -> GrTag -> ([RunVal],Bool)
grEvalTag rs t
  =  case t of
        GrTag_Con       _ i _    ->  ([RVCat NdCon,RVInt i],szYes)
        GrTag_Hole               ->  ([RVCat NdHole],szNo)
        GrTag_Rec                ->  ([RVCat NdRec,RVInt 0],szYes)
        GrTag_Fun           n    ->  ([RVCat NdFun,rsVar rs n],szNo)
        GrTag_App           _    ->  ([RVCat NdApp],szNo)
        GrTag_PApp nMiss    n    ->  ([RVCat NdPApp,RVInt nMiss,rsVar rs n],szNo)
  where  szNo   = False -- const []
         szYes  = True  -- (:[])
%%]

%%[8
grEvalVal :: RunState -> GrVal -> RunVal
grEvalVal rs v
  =  case v of
        GrVal_Empty             ->  RVNil
        GrVal_LitInt    i       ->  RVInt i
        GrVal_LitStr    s       ->  RVStr s
        GrVal_Tag  t            ->  error "tag only variables not implemented"
        GrVal_Var       n       ->  rsVar rs n
        GrVal_Node t    fL      ->  case grEvalTag rs t of
                                        (tgL,True ) -> mkRN (tgL ++ [RVInt (length fL)] ++ vL)
                                        (tgL,False) -> mkRN (tgL ++                        vL)
                                where vL = map (grEvalVal rs) fL
%%[[10                               
        GrVal_NodeAdapt r adL   ->  case rsVar rs r of
                                        RVNode a
                                          ->  case elems a of
                                                (c:t:_:fL)
                                                  ->  mkRN (c:t:RVInt (length fL'):fL')
                                                      where  fL' = ad adL fL 0
                                                             ad (GrAdapt_Ins o v:adL') fL fO | o' == fO
                                                                =  grEvalVal rs v : fL'
                                                                   where  fL' = ad adL' fL fO
                                                                          (RVInt o') = grEvalVal rs o
                                                             ad (GrAdapt_Upd o v:adL') (_:fL) fO | o' == fO
                                                                =  grEvalVal rs v : fL'
                                                                   where  fL' = ad adL' fL (fO+1)
                                                                          (RVInt o') = grEvalVal rs o
                                                             ad (GrAdapt_Del o:adL') (_:fL) fO | o' == fO
                                                                =  fL'
                                                                   where  fL' = ad adL' fL (fO+1)
                                                                          (RVInt o') = grEvalVal rs o
                                                             ad adL (f:fL) fO
                                                                =  f : fL'
                                                                   where  fL' = ad adL fL (fO+1)
                                                             ad [] fL _
                                                                =  fL
%%]]                                                                
%%]

%%[8
grCall :: RunState -> RunVal -> [RunVal] -> RunState
grCall rs f aL
  =  case f of
        RVGlob _ nL e
          ->  rs'
              where  re = Map.fromList (zip nL aL) `Map.union` rsGlobEnv rs
                     rs' = rs {rsEnv = re, rsNext = Just e}
%%]

%%[8
grFFI :: RunState -> String -> [RunVal] -> IO (RunState,Maybe RunVal)
grFFI rs f aL
  =  case Map.lookup f primMp of
        Just f'
            ->  f' rs aL
        _   ->  do  { rs' <- halt rs ("No ffi for:" >#< f >-< indent 2 ("with args:" >#< (ppBracketsCommas . map pp $ aL)))
                    ; return (rs',Nothing)
                    }
%%]

%%[8
grEvalApp :: RunState -> RunVal -> GrValL -> IO (RunState,Maybe RunVal)
grEvalApp rs f aL
  =  let  aLSz = length aL
     in   case f of
            RVNode a
              ->  case elems a of
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL) | aLSz > ndMiss
                        ->  do  {  ndF' <- rsDeref rs' ndF
                                ;  return (grCall rs' ndF' (ndAL ++ (map (grEvalVal rs) (take ndMiss aL))),Nothing)
                                }
                            where  n = hsnWild
                                   e = GrExpr_App n (drop ndMiss aL)
                                   stk = (Left $ GrPatLam_Var n,e,rsEnv rs) : rsStack rs
                                   rs' = rs {rsStack = stk, rsNext = Just e}
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL) | aLSz == ndMiss
                        ->  do  {  ndF' <- rsDeref rs ndF
                                ;  return (grCall rs ndF' (ndAL ++ (map (grEvalVal rs) aL)),Nothing)
                                }
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL)
                        ->  return (rs,Just (mkRN ([RVCat NdPApp,RVInt (ndMiss-aLSz),ndF] ++ ndAL ++ (map (grEvalVal rs) aL))))
                    _   ->  return (rs,Just f)
%%]

%%[8
rsCheckMem :: RunState -> IO RunState
rsCheckMem rs
  =  let  h = rsHeap rs
          m = rhMem h
     in   if rhFree h < rhSize h
          then  return rs
          else  do  {  let newSz = 3 * rhSize h `div` 2
                    ;  elts <- getElems m
                    ;  let elts' = elts ++ replicate (newSz - rhSize h) RVNil
                    ;  m' <- newListArray (0,newSz-1) elts'
                    ;  let h' = h {rhSize = newSz, rhMem = m'}
                    ;  return (rs {rsHeap = h'})
                    }
%%]

%%[8
grEvalExpr :: RunState -> GrExpr -> IO (RunState,Maybe RunVal)
grEvalExpr rs e
  =  case e of
        GrExpr_Unit v
          ->  return (rs,Just (grEvalVal rs v))
        GrExpr_UpdateUnit n v
          ->  do  {  let  (RVPtr p) = rsVar rs n
                          rv = grEvalVal rs v
                  ;  rv `seq` writeArray (rhMem . rsHeap $ rs) p rv
                  ;  return (rs,Just rv)
                  }
        GrExpr_FetchUpdate ns nd
          ->  do  {  ns' <- rsVarDeref rs ns
                  ;  let (RVPtr pd) = rsVar rs nd
                  ;  ns' `seq` writeArray (rhMem . rsHeap $ rs) pd ns'
                  ;  return (rs,Just RVNil)
                  }
        GrExpr_FetchNode n
          ->  do  {  n' <- rsVarDeref rs n
                  ;  return (rs,Just n')
                  }
        GrExpr_FetchField n offset _
          ->  do  {  (RVNode n') <- rsVarDeref rs n
                  ;  error "fetch with offset not supported yet"
                  ;  return (rs,Just $ n' ! offset)
                  }
        GrExpr_Store v
          ->  do  {  rs2 <- rsCheckMem rs
                  ;  let  h = rsHeap rs2
                          m = rhMem h
                          p = rhFree h
                          rv = grEvalVal rs v
                  ;  rv `seq` writeArray (rhMem h) p rv
                  ;  return (rs2 {rsHeap = h {rhFree = p+1}},Just (RVPtr p))
                  }
        GrExpr_Call fn aL
          ->  do  {  fn' <- rsVarDeref rs fn
                  ;  return (grCall rs fn' (map (grEvalVal rs) aL),Nothing)
                  }
        GrExpr_FFI fn aL _
          ->  grFFI rs fn (map (grEvalVal rs) aL)
        GrExpr_App fn aL
          ->  grEvalApp rs (rsVar rs fn) aL
        GrExpr_Eval n
          ->  let  upd rs n
                     =  let  n2 = hsnWild
                             e = GrExpr_UpdateUnit n (GrVal_Var n2)
                             stk = (Left $ GrPatLam_Var n2,e,rsEnv rs) : rsStack rs
                        in   rs {rsStack = stk, rsNext = Just e}
              in   do  {  n' <- rsVarDeref rs n
                       ;  case n' of
                            v@(RVNode a)
                              ->  case elems a of
                                    (RVCat NdFun:ndF:ndAL)
                                      ->  do  {  ndF' <- rsDeref rs ndF
                                              ;  return (grCall (upd rs n) ndF' ndAL,Nothing)
                                              }
                                    (RVCat NdApp:ndFAL)
                                      ->  return (rs3,Nothing)
                                          where  rs2 = upd rs n
                                                 n2 = hsnWild
                                                 nL@(nF:nAL) = take (length ndFAL) hsnLclSupply
                                                 e = GrExpr_Seq (GrExpr_Eval nF) (GrPatLam_Var n2) (GrExpr_App n2 (map GrVal_Var nAL))
                                                 re = Map.fromList (zip nL ndFAL) `Map.union` rsGlobEnv rs
                                                 rs3 = rs2 {rsNext = Just e, rsEnv = re}
                                    _ ->  return (rs,Just v)
                            v@RVNil
                              ->  do  { rs' <- halt rs ("Cannot be evaluated:" >#< pp v >-< indent 2 ("in:" >#< ppGrExpr e))
                                      ; return (rs',Nothing)
                                      }
                            v ->  return (rs,Just v)
                       }
        GrExpr_Case v altL
          ->  case grEvalVal rs v of
                nd@(RVNode a)
                  ->  case elems a of
                        (RVCat NdCon:RVInt ndTg:_)
                          ->  let  lookup t []    = Nothing
                                   lookup t (GrAlt_Alt p@(GrPatAlt_Node (GrTag_Con _ t' _) _) e:aL)
                                     | t == t'    = Just (\rs -> grPatBind rs (rsEnv rs) nd (Right p),e)
%%[[10                                     
                                   lookup t (GrAlt_Alt p@(GrPatAlt_NodeSplit (GrTag_Con _ t' _) _ _) e:aL)
                                     | t == t'    = Just (\rs -> grPatBind rs (rsEnv rs) nd (Right p),e)
%%]]                                     
                                   lookup t (_:aL)= lookup t aL
                              in   case lookup ndTg altL of
                                     Just (extRE,e)
                                       ->  return (rs',Nothing)
                                           where  re = extRE rs
                                                  rs'= rs {rsEnv = re, rsNext = Just e}
                                     Nothing
                                       ->  errNoAlt nd
                        (RVCat NdRec:_)
                          ->  return (rs',Nothing)
                              where  (GrAlt_Alt p e:_) = altL
                                     re = grPatBind rs (rsEnv rs) nd (Right p)
                                     rs'= rs {rsEnv = re, rsNext = Just e}
                nd@(RVInt v)
                  ->  case lookup v altL of
                        Just (extRE,e)
                          -> return (rs',Nothing)
                             where re = extRE rs
                                   rs' = rs {rsEnv = re, rsNext = Just e}
                        Nothing
                          -> errNoAlt nd
                  where  lookup v []    = Nothing
                         lookup v (GrAlt_Alt p@(GrPatAlt_LitInt v') e : _)
                           | v == v'    = Just (rsEnv,e)
                         lookup v (GrAlt_Alt p@(GrPatAlt_Otherwise) e : _)
                                        = Just (\rs -> grPatBind rs (rsEnv rs) nd (Right p),e)
                         lookup v (_:aL)= lookup v aL
          where  errNoAlt v
                   = do  { rs' <- halt rs ("No case alt for:" >#< pp v >-< indent 2 ("in:" >#< ppGrExpr e))
                         ; return (rs',Nothing)
                         }
%%]

%%[8

fromGrVar_Var :: GrVar -> HsName
fromGrVar_Var (GrVar_Var v) = v

grPatBind :: RunState -> RunEnv -> RunVal -> GrPat -> RunEnv
grPatBind rs re v p
  =  case p of
        Left   GrPatLam_Empty
          ->  re
        Right (GrPatAlt_LitInt _)
          ->  re
        Right (GrPatAlt_Tag _)
          ->  re
        Left  (GrPatLam_Var n)
          ->  Map.insert n v re
        Right (GrPatAlt_Otherwise)
          ->  re
        Right (GrPatAlt_Node GrTag_Unboxed (pf:_))
          ->  Map.insert pf v re
        Right (GrPatAlt_Node _ pfL)
          ->  case v of
                RVNode a
                  ->  case elems a of
                        (RVCat _:_:_:vfL)
                          ->  Map.fromList (zip pfL vfL) `Map.union` re
        Left  (GrPatLam_VarNode ((GrVar_KnownTag GrTag_Unboxed):(GrVar_Var pf:_)))
          ->  Map.insert pf v re
        Left  (GrPatLam_VarNode (_:pfL))
          ->  case v of
                RVNode a
                  ->  case elems a of
                        (RVCat _:_:_:vfL)
                          ->  Map.fromList (zip (map fromGrVar_Var pfL) vfL) `Map.union` re
%%[[10          
        Right (GrPatAlt_NodeSplit _ rNm splL)
          ->  case v of
                RVNode a
                  ->  case elems a of
                        (c@(RVCat _):t:_:vfL)
                          ->  Map.singleton rNm (mkRN (c:t:RVInt (length r):r)) `Map.union` Map.fromList bL `Map.union` re
                              where  (r,bL) = spl splL vfL 0
                                     spl (GrSplit_Sel n o:splL) (f:fL) fO | o' == fO
                                        =  (rfL,(n,f):bfL)
                                           where  (rfL,bfL) = spl splL fL (fO+1)
                                                  (RVInt o') = grEvalVal rs o
                                     spl splL (f:fL) fO
                                        =  (f:rfL,bfL)
                                           where  (rfL,bfL) = spl splL fL (fO+1)
                                     spl [] fL fO
                                        =  (fL,[])
%%]]                                        
%%]

%%[8
halt :: RunState -> PP_Doc -> IO RunState
halt rs p = return (rs {rsHalted = Just p})
%%]

%%[8
run1Expr :: RunState -> GrExpr -> IO RunState
run1Expr rs e
  =  do  {  rse <- grEvalExpr rs e
         ;  case rse of
              (rs,Just v) | null (rsStack rs)
                  ->  do  {  v' <- rsDeref rs v
                          ;  halt rs  ("Terminate ("
                                      >|< rsNrSteps rs >#< "steps,"
                                      >#< rhFree (rsHeap rs) >|< (ppParens . pp . rhSize . rsHeap $ rs) >#< "nodes) with:"
                                      >-< indent 2 (pp v')
                                      )
                          }
              (rs,Just v)
                  ->  return rs2
                      where  ((np,ne,re):stk) = rsStack rs
                             re' = v `seq` grPatBind rs re v np
                             rs2 = rs {rsEnv = re', rsNext = Just ne, rsStack = stk}
              (rs,_)
                    ->  return rs
         }
%%]

%%[8
run1Step :: RunState -> IO RunState
run1Step rs
  =  do  {  case rsNext rs of
              Nothing
                ->  halt rs (pp "No (more) instructions")
              Just e
                ->  case e of
                      GrExpr_Seq e1 p e2
                          ->  run1Expr rs' e1
                              where  stk = (Left p,e2,rsEnv rs) : rsStack rs
                                     rs' = rs {rsNext = Just e2, rsStack = stk, rsNrSteps = rsNrSteps rs + 1}
                      _   ->  run1Expr (rs {rsNrSteps = rsNrSteps rs + 1}) e
         }
%%]

