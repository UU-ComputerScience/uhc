% $Id: GRI.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run GRI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module GRIRun import(EHCommon,GRICommon,GrinCode,GrinCodePretty)
%%]

%%[8 import(FiniteMap,Maybe,Data.Array,Data.Array.IO)
%%]

%%[8 import(UU.Pretty)
%%]

%%[8 export(RunState(..),RunHeap(..),RunVal(..),RunLoc,RunEnv,run1Step,mkRN,NdCat(..))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Machine values, memory
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
type RunLoc = Maybe GrExpr

data NdCat = NdCon | NdApp | NdFun | NdPApp deriving (Enum,Eq,Ord,Show)

data RunVal
  =  RVNil
  |  RVCat !NdCat
  |  RVInt !Int
  |  RVNode !(Array Int RunVal)
  |  RVPtr !Int
  |  RVGlob !HsName ![HsName] !GrExpr

data RunHeap = RunHeap {rhMem :: !(FiniteMap Int RunVal), rhSize :: !Int, rhFree :: !Int}

type RunEnv = FiniteMap HsName RunVal

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

mkRN :: [RunVal] -> RunVal
mkRN es = RVNode (listArray (0,length es - 1) es)

rsVar :: RunState -> HsName -> RunVal
rsVar rs = lookupWithDefaultFM (rsEnv rs) RVNil

rsDeref :: RunState -> RunVal -> RunVal
rsDeref rs r
  =  case r of
        RVPtr p -> lookupWithDefaultFM (rhMem . rsHeap $ rs) RVNil p
        _ -> r

rsVarDeref :: RunState -> HsName -> RunVal
rsVarDeref rs
  =  rsDeref rs . rsVar rs

instance Show RunVal where
  show v = "VAL"

instance PP RunVal where
  pp (RVNil             ) = pp "-:-"
  pp (RVCat     v       ) = "C:" >|< pp (show v)
  pp (RVInt     v       ) = "I:" >|< pp v
  pp (RVNode    v       ) = "N:" >|< ppListSep "(" ")" " " (elems v)
  pp (RVPtr     v       ) = "P:" >|< pp v
  pp (RVGlob    v _ _   ) = "G:" >|< pp v

instance Show RunHeap where
  show h = "HEAP"

instance PP RunHeap where
  pp h =  "HEAP sz=" >|< rhSize h >|< " fr=" >|< rhFree h
          >-< indent 2 (ppFM . rhMem $ h)

instance Show RunState where
  show h = "STATE"

instance PP RunState where
  pp s =  "STATE next=" >|< maybe empty ppGrExpr (rsNext s)
          >-< indent 2
                (pp (rsHeap s)
                 >-< "ENV" >#< (ppFM . rsEnv $ s)
                 >-< "STK" >#< (vlist . map (\(p,s,_) -> ppGrPat p >#< "->" >#< ppGrExpr s) . rsStack $ s)
                )
%%]

%%[8
ppFM :: (PP k,PP v) => FiniteMap k v -> PP_Doc
ppFM = pp_block "[" "]" "," . map ppPair . fmToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Machine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
grEvalTag :: RunState -> GrTag -> [RunVal]
grEvalTag rs t
  =  case t of
        GrTag_Lit GrTagCon           i _    ->  [RVCat NdCon,RVInt i]
        GrTag_Lit GrTagFun           i n    ->  [RVCat NdFun,rsVar rs n]
        GrTag_Lit GrTagApp           _ _    ->  [RVCat NdApp]
        GrTag_Lit (GrTagPApp nMiss)  _ _    ->  [RVCat NdPApp,RVInt nMiss]
        GrTag_None                          ->  [RVCat NdCon,RVInt 0]
%%]

%%[8
grEvalVal :: RunState -> GrVal -> RunVal
grEvalVal rs v
  =  case v of
        GrVal_Node t    fL  ->  mkRN (grEvalTag rs t ++ map (grEvalVal rs) fL)
        GrVal_LitInt    i   ->  RVInt i
        GrVal_Var       n   ->  rsVar rs n
        GrVal_Empty         ->  RVNil
%%]

%%[8
grCall :: RunState -> RunVal -> [RunVal] -> RunState
grCall rs f aL
  =  case f of
        RVGlob _ nL e
          ->  rs'
              where  re = rsGlobEnv rs `plusFM` listToFM (zip nL aL)
                     rs' = rs {rsEnv = re, rsNext = Just e}
%%]

%%[8
grFFI :: RunState -> String -> [RunVal] -> IO (RunState,Maybe RunVal)
grFFI rs f aL
  =  case (f,aL) of
        ("primAddInt",[RVInt i1,RVInt i2])              ->  return (rs,Just (RVInt (i1 + i2)))
        ("primDivInt",[RVInt i1,RVInt i2])              ->  return (rs,Just (RVInt (i1 `div` i2)))
        ("primMulInt",[RVInt i1,RVInt i2])              ->  return (rs,Just (RVInt (i1 * i2)))
        ("primSubInt",[RVInt i1,RVInt i2])              ->  return (rs,Just (RVInt (i1 - i2)))
        ("primCmpInt",[RVInt i1,RVInt i2])              ->  return (rs,Just (mkRN [RVCat NdCon,RVInt c]))
                                                            where c = case i1 `compare` i2 of {EQ->0; GT->1; LT->2}
        _   ->  do  { rs' <- halt rs ("No ffi for:" >#< f >-< indent 2 ("with args:" >#< (ppCommaList . map pp $ aL)))
                    ; return (rs',Nothing)
                    }
%%]

; $_EQ 
    = { unit (#0/C/$_EQ)} 
; $_GT 
    = { unit (#1/C/$_GT)} 
; $_LT 
    = { unit (#2/C/$_LT)} 

       primCmpInt =
        new Function2("primCmpInt") {
         public Object eval2
                 (final Object i1,final Object i2) {
          final int
           _i1 =
            ((Int)eval(i1)).intValue() ;
          final int
           _i2 =
            ((Int)eval(i2)).intValue() ;
          if (_i1 < _i2) return new Object[]{new Int(2)} ; else if (_i1 > _i2) return new Object[]{new Int(1)}; else return new Object[]{new Int(0)} ;
         }
        } ;

        unit (#0/C/$_Ordering $__1)


%%[8
grEvalApp :: RunState -> RunVal -> GrValL -> (RunState,Maybe RunVal)
grEvalApp rs f aL
  =  let  aLSz = length aL
     in   case f of
            RVNode a
              ->  case elems a of
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL) | aLSz > ndMiss
                        ->  (grCall rs' (rsDeref rs' ndF) (ndAL ++ (map (grEvalVal rs) (take ndMiss aL))),Nothing)
                            where  n = hsnWild
                                   e = GrExpr_App n (drop ndMiss aL)
                                   stk = (GrPat_Var n,e,rsEnv rs) : rsStack rs
                                   rs' = rs {rsStack = stk, rsNext = Just e}
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL) | aLSz == ndMiss
                        ->  (grCall rs (rsDeref rs ndF) (ndAL ++ (map (grEvalVal rs) aL)),Nothing)
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL)
                        ->  (rs,Just (mkRN ([RVCat NdPApp,RVInt (ndMiss-aLSz),ndF] ++ ndAL ++ (map (grEvalVal rs) aL))))
                    _   ->  (rs,Just f)
%%]

%%[8
grEvalExpr :: RunState -> GrExpr -> IO (RunState,Maybe RunVal)
grEvalExpr rs e
  =  case e of
        GrExpr_Unit v
          ->  return (rs,Just (grEvalVal rs v))
        GrExpr_Store v
          ->  return (rs',Just (RVPtr p))
              where  rv = grEvalVal rs v
                     h = rsHeap rs
                     p = rhFree h
                     p2 = p + 1
                     m = rv `seq` addToFM (rhMem h) p rv
                     h' = h {rhFree = p2, rhSize = p2, rhMem = m}
                     rs' = rs {rsHeap = h'}
        GrExpr_Update n v
          ->  return (rs',Just RVNil)
              where  (RVPtr p) = rsVar rs n
                     rv = grEvalVal rs v
                     h = rsHeap rs
                     m = rv `seq` addToFM (rhMem h) p rv
                     rs' = rs {rsHeap = h {rhMem = m}}
        GrExpr_Call fn aL
          ->  return (grCall rs (rsVarDeref rs fn) (map (grEvalVal rs) aL),Nothing)
        GrExpr_FFI fn aL
          ->  grFFI rs fn (map (rsVar rs) aL)
        GrExpr_App fn aL
          ->  return (grEvalApp rs (rsVar rs fn) aL)
        GrExpr_Eval n
          ->  let  upd rs n
                     =  let  n2 = hsnWild
                             e = GrExpr_Seq (GrExpr_Update n (GrVal_Var n2)) (GrPat_Empty) (GrExpr_Unit (GrVal_Var n2))
                             stk = (GrPat_Var n2,e,rsEnv rs) : rsStack rs
                        in   rs {rsStack = stk, rsNext = Just e}
              in   case rsVarDeref rs n of
                     v@(RVNode a)
                       ->  case elems a of
                             (RVCat NdFun:ndF:ndAL)
                               ->  return (grCall (upd rs n) (rsDeref rs ndF) ndAL,Nothing)
                             (RVCat NdApp:ndFAL)
                               ->  return (rs3,Nothing)
                                   where  rs2 = upd rs n
                                          n2 = hsnWild
                                          nL@(nF:nAL) = take (length ndFAL) grLclNmSupplyL
                                          e = GrExpr_Seq (GrExpr_Eval nF) (GrPat_Var n2) (GrExpr_App n2 (map GrVal_Var nAL))
                                          re = rsGlobEnv rs `plusFM` listToFM (zip nL ndFAL)
                                          rs3 = rs2 {rsNext = Just e, rsEnv = re}
                             _ ->  return (rs,Just v)
                     v ->  return (rs,Just v)
        GrExpr_Case v altL
          ->  case grEvalVal rs v of
                nd@(RVNode a)
                  ->  case elems a of
                        (RVCat NdCon:RVInt ndTg:ndAL)
                          ->  let  lookup t []    = Nothing
                                   lookup t (GrAlt_Alt (GrPat_Node (GrTag_Lit _ t' _) fL) e:aL)
                                     | t == t'    = Just (fL,e)
                                     | otherwise  = lookup t aL
                                   lookup t (GrAlt_Alt (GrPat_Node GrTag_None fL) e:aL)
                                     | t == 0     = Just (fL,e)
                                     | otherwise  = Nothing
                              in   case lookup ndTg altL of
                                     Just (fL,e)
                                       ->  return (rs',Nothing)
                                           where  re = rsEnv rs `plusFM` listToFM (zip fL ndAL)
                                                  rs'= rs {rsEnv = re, rsNext = Just e}
                                     Nothing
                                       ->  do  { rs' <- halt rs ("No case alt for:" >#< pp nd >-< indent 2 ("in:" >#< ppGrExpr e))
                                               ; return (rs',Nothing)
                                               }
%%]

%%[8
grPatBind :: RunEnv -> RunVal -> GrPat -> RunEnv
grPatBind re v p
  =  case p of
        GrPat_Var n
          -> addToFM re n v
        GrPat_Empty
          -> re
        GrPat_Node pt pfL
          ->  case v of
                RVNode a
                  ->  case elems a of
                        (RVCat NdCon:vt:vfL) -> re `plusFM` listToFM (zip pfL vfL)
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
                  ->  halt rs ("Terminate (after" >#< rsNrSteps rs >#< "steps) with:" >-< indent 2 (pp . rsDeref rs $ v))
              (rs,Just v)
                  ->  return rs2
                      where  ((np,ne,re):stk) = rsStack rs
                             re' = v `seq` grPatBind re v np
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
                              where  stk = (p,e2,rsEnv rs) : rsStack rs
                                     rs' = rs {rsNext = Just e2, rsStack = stk, rsNrSteps = rsNrSteps rs + 1}
                      _   ->  run1Expr (rs {rsNrSteps = rsNrSteps rs + 1}) e
         }
%%]

