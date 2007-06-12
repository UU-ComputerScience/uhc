%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Aspect of name, for use by HS -> EH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}NameAspect} import({%{EH}Base.Common},{%{EH}Base.Builtin},qualified {%{EH}EH} as EH)
%%]

%%[1 import(EH.Util.Pretty)
%%]

%%[1 export(IdDefOcc(..),emptyIdDefOcc,mkIdDefOcc)
%%]

%%[8 import({%{EH}Base.CfgPP})
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Aspects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs export(IdAspect(..))
data IdAspect
  = IdAsp_Val_Var
  | IdAsp_Val_Pat       {iaspDecl   ::  EH.Decl                         }
  | IdAsp_Val_Fun       {iaspPatL   :: [EH.PatExpr], iaspBody :: EH.Expr, iaspUniq :: !UID}
  | IdAsp_Val_Sig       {iaspDecl   ::  EH.Decl                         }
  | IdAsp_Val_Fix
  | IdAsp_Val_Con
%%[[5
  | IdAsp_Val_Fld
%%]]
  | IdAsp_Type_Con
%%[[3
  | IdAsp_Type_Var
%%]]
%%[[5
  | IdAsp_Type_Def      {iaspDecl   :: !EH.Decl                         }
%%]]
%%[[6
  | IdAsp_Type_Sig      {iaspDecl   :: !EH.Decl                         }
  | IdAsp_Kind_Con
  | IdAsp_Kind_Var
%%]]
%%[[8
  | IdAsp_Val_FFI       {iaspDecl   :: !EH.Decl                         }
%%]]
%%[[9
  | IdAsp_Class_Class
  | IdAsp_Class_Def     {iaspDecl   :: !EH.Decl, iaspDeclInst :: !EH.Decl}
  | IdAsp_Inst_Inst
  | IdAsp_Inst_Def      {iaspDecl   ::  EH.Decl, iaspClassNm  :: !HsName}
  | IdAsp_Dflt_Def      {iaspDecl   :: !EH.Decl                         }
%%]]
  | IdAsp_Any
%%]

%%[1 hs export(iaspIsValSig)
iaspIsValSig :: IdAspect -> Bool
iaspIsValSig (IdAsp_Val_Sig _) = True
iaspIsValSig _                 = False
%%]

%%[1 hs export(iaspIsFun)
iaspIsFun :: IdAspect -> Bool
iaspIsFun (IdAsp_Val_Fun _ _ _) = True
iaspIsFun _                     = False
%%]

%%[1 hs
instance Show IdAspect where
  show _ = "IdAspect"
%%]

%%[1 hs
instance PP IdAspect where
  pp  IdAsp_Val_Var         = pp "VAR"
  pp (IdAsp_Val_Pat _    )  = pp "PAT"
  pp (IdAsp_Val_Fun _ _ _)  = pp "FUN"
  pp (IdAsp_Val_Sig _    )  = pp "SIG"
  pp  IdAsp_Val_Fix         = pp "FIX"
  pp  IdAsp_Val_Con         = pp "CON"
%%[[5
  pp  IdAsp_Val_Fld         = pp "FLD"
%%]
  pp  IdAsp_Type_Con        = pp "CON"
%%[[3
  pp  IdAsp_Type_Var        = pp "VAR"
%%]
%%[[5
  pp (IdAsp_Type_Def _   )  = pp "DEF"
%%]
%%[[6
  pp (IdAsp_Type_Sig _   )  = pp "SIG"
  pp  IdAsp_Kind_Con        = pp "CON"
  pp  IdAsp_Kind_Var        = pp "VAR"
%%]
%%[[8
  pp (IdAsp_Val_FFI _    )  = pp "FFI"
%%]
%%[[9
  pp  IdAsp_Class_Class     = pp "CLS"
  pp (IdAsp_Class_Def _ _)  = pp "DEF"
  pp  IdAsp_Inst_Inst       = pp "INS"
  pp (IdAsp_Inst_Def  _ _)  = pp "DEF"
  pp (IdAsp_Dflt_Def  _  )  = pp "DEF"
%%]
  pp  IdAsp_Any             = pp "ANY"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defining occurrence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs
data IdDefOcc
  = IdDefOcc
      { doccOcc     :: !IdOcc
      , doccAsp     :: !IdAspect
      , doccLev     :: !NmLev
      , doccRange   :: !Range
%%[[20
      , doccNmAlts  :: !(Maybe [HsName])
%%]
      }
  deriving (Show)

emptyIdDefOcc :: IdDefOcc
emptyIdDefOcc = mkIdDefOcc (IdOcc hsnUnknown IdOcc_Any) IdAsp_Any nmLevAbsent emptyRange
%%]

%%[1.mkIdDefOcc hs
mkIdDefOcc :: IdOcc -> IdAspect -> NmLev -> Range -> IdDefOcc
mkIdDefOcc o a l r = IdDefOcc o a l r
%%]

%%[20 -1.mkIdDefOcc hs
mkIdDefOcc :: IdOcc -> IdAspect -> NmLev -> Range -> IdDefOcc
mkIdDefOcc o a l r = IdDefOcc o a l r Nothing
%%]

%%[1
instance PP IdDefOcc where
  pp o = doccOcc o >|< "/" >|< doccAsp o >|< "/" >|< doccLev o
%%[[20
         >|< maybe empty (\ns -> "/" >|< ppBracketsCommas ns) (doccNmAlts o)
%%]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance ForceEval IdAspect where
  forceEval x@(IdAsp_Val_Pat d    ) | d `seq` True = x
  forceEval x@(IdAsp_Val_Fun p d i) | p `seq` d `seq` forceEval i `seq` True = x
  forceEval x@(IdAsp_Val_Sig d    ) | d `seq` True = x
  forceEval x@(IdAsp_Inst_Def d n ) | d `seq` True = x
  forceEval x                       = x
%%[[101
  fevCount x | x `seq` True = cm1 "IdAspect_*"
%%]]

instance ForceEval IdDefOcc where
  forceEval x@(IdDefOcc o a l r as) | forceEval a `seq` forceEval as `seq` forceEval r `seq` True = x
%%[[101
  fevCount (IdDefOcc o a l r as) = cmUnions [cm1 "IdDefOcc",fevCount o,fevCount a,fevCount as,fevCount r,fevCount l]
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PP
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ppIdOcc)
-- intended for parsing
ppIdOcc :: CfgPP x => x -> IdOcc -> PP_Doc
ppIdOcc x o = ppCurlysCommas [cfgppHsName x (ioccNm o),pp (ioccKind o)]
%%]

%%[1.PP.IdOcc
instance PP IdOcc where
  pp o = ppCurlysCommas [pp (ioccNm o),pp (ioccKind o)]
%%]

%%[8 -1.PP.IdOcc
instance PP IdOcc where
  pp = ppIdOcc CfgPP_Plain
%%]

%%[20
instance PPForHI IdOcc where
  ppForHI = ppIdOcc CfgPP_HI
%%]

