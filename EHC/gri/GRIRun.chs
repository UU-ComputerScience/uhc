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

%%[8 export(RunState(..),RunHeap(..),RunVal(..),RunLoc,RunEnv,run1Step)
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

grEvalVal :: RunState -> GrVal -> RunVal
grEvalVal rs v
  =  case v of
        GrVal_Node t    fL  ->  mkRN (grEvalTag rs t ++ map (grEvalVal rs) fL)
        GrVal_LitInt    i   ->  RVInt i
        GrVal_Var       n   ->  rsVar rs n
        GrVal_Empty         ->  RVNil

grCall :: RunState -> RunVal -> [RunVal] -> RunState
grCall rs f aL
  =  case f of
        RVGlob _ nL e
          ->  rs'
              where  re = rsGlobEnv rs `plusFM` listToFM (zip nL aL)
                     rs' = rs {rsEnv = re, rsNext = Just e}

grEvalApp :: RunState -> RunVal -> GrValL -> (RunState,Maybe RunVal)
grEvalApp rs f aL
  =  let  aLSz = length aL
     in   case f of
            RVNode a
              ->  case elems a of
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL) | aLSz > ndMiss
                        ->  (grCall rs' (rsDeref rs' ndF) (ndAL ++ (map (grEvalVal rs) (take ndMiss aL))),Nothing)
                            where  n = HNm "_"
                                   e = GrExpr_App n (drop ndMiss aL)
                                   stk = (GrPat_Var n,e,rsEnv rs) : rsStack rs
                                   rs' = rs {rsStack = stk, rsNext = Just e}
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL) | aLSz == ndMiss
                        ->  (grCall rs (rsDeref rs ndF) (ndAL ++ (map (grEvalVal rs) aL)),Nothing)
                    (RVCat NdPApp:RVInt ndMiss:ndF:ndAL)
                        ->  (rs,Just (mkRN ([RVCat NdPApp,RVInt (ndMiss-aLSz),ndF] ++ ndAL ++ (map (grEvalVal rs) aL))))
                    _   ->  (rs,Just f)

grEvalExpr :: RunState -> GrExpr -> (RunState,Maybe RunVal)
grEvalExpr rs e
  =  case e of
        GrExpr_Unit v
          ->  (rs,Just (grEvalVal rs v))
        GrExpr_Store v
          ->  (rs',Just (RVPtr p))
              where  rv = grEvalVal rs v
                     h = rsHeap rs
                     p = rhFree h
                     p2 = p + 1
                     m = rv `seq` addToFM (rhMem h) p rv
                     h' = h {rhFree = p2, rhSize = p2, rhMem = m}
                     rs' = rs {rsHeap = h'}
        GrExpr_Update n v
          ->  (rs',Just RVNil)
              where  (RVPtr p) = rsVar rs n
                     rv = grEvalVal rs v
                     h = rsHeap rs
                     m = rv `seq` addToFM (rhMem h) p rv
                     rs' = rs {rsHeap = h {rhMem = m}}
        GrExpr_Call fn aL
          ->  (grCall rs (rsVarDeref rs fn) (map (grEvalVal rs) aL),Nothing)
        GrExpr_App fn aL
          ->  grEvalApp rs (rsVar rs fn) aL
        GrExpr_Eval n
          ->  let  upd rs n
                     =  let  n2 = HNm "_"
                             e = GrExpr_Seq (GrExpr_Update n (GrVal_Var n2)) (GrPat_Empty) (GrExpr_Unit (GrVal_Var n2))
                             stk = (GrPat_Var n2,e,rsEnv rs) : rsStack rs
                        in   rs {rsStack = stk, rsNext = Just e}
              in   case rsVarDeref rs n of
                     v@(RVNode a)
                       ->  case elems a of
                             (RVCat NdFun:ndF:ndAL)
                               ->  (grCall (upd rs n) ndF ndAL,Nothing)
                             (RVCat NdApp:ndFAL)
                               ->  (rs3,Nothing)
                                   where  rs2 = upd rs n
                                          n2 = HNm "_"
                                          nL@(nF:nAL) = map (\i -> HNm ("_" ++ show i)) [1..length ndFAL]
                                          e = GrExpr_Seq (GrExpr_Eval nF) (GrPat_Var n2) (GrExpr_App n2 (map GrVal_Var nAL))
                                          re = rsGlobEnv rs `plusFM` listToFM (zip nL ndFAL)
                                          rs3 = rs2 {rsNext = Just e, rsEnv = re}
                             _ ->  (rs,Just v)
                     v ->  (rs,Just v)

grPatBind :: RunEnv -> RunVal -> GrPat -> RunEnv
grPatBind re v p
  =  case p of
        GrPat_Var n
          -> addToFM re n v
        GrPat_Empty
          -> re

halt :: RunState -> PP_Doc -> IO RunState
halt rs p = return (rs {rsHalted = Just p})

run1Expr :: RunState -> GrExpr -> IO RunState
run1Expr rs e
  =  case grEvalExpr rs e of
        (rs,Just v) | null (rsStack rs)
            ->  halt rs ("Terminate with:" >-< indent 2 (pp . rsDeref rs $ v))
        (rs,Just v)
            ->  return rs2
                where  ((np,ne,re):stk) = rsStack rs
                       re' = v `seq` grPatBind re v np
                       rs2 = rs {rsEnv = re', rsNext = Just ne, rsStack = stk}
        (rs,_)
            ->  return rs

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
                                     rs' = rs {rsNext = Just e2, rsStack = stk}
                      _   ->  run1Expr rs e
         }
%%]

