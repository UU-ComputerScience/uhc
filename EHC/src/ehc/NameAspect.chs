%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Aspect of name, for use by HS -> EH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}NameAspect} import(UU.Pretty,EH.Util.PPUtils,{%{EH}Base.Common},qualified {%{EH}EH} as EH) export(IdAspect(..),iaspIsSig,iaspIsFun)
%%]

%%[1 export(IdDefOcc(..),emptyIdDefOcc,mkIdDefOcc)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Aspects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs
data IdAspect
  = IdAsp_Val_Var
  | IdAsp_Val_Pat       {iaspDecl   ::  EH.Decl                         }
  | IdAsp_Val_Fun       {iaspPatL   :: [EH.PatExpr], iaspBody :: EH.Expr}
  | IdAsp_Val_Sig       {iaspDecl   ::  EH.Decl                         }
  | IdAsp_Val_Fix
  | IdAsp_Val_Con
%%[[5
  | IdAsp_Val_Fld
%%]
  | IdAsp_Type_Con
%%[[3
  | IdAsp_Type_Var
%%]
%%[[5
  | IdAsp_Type_Def      {iaspDecl   ::  EH.Decl                         }
%%]
%%[[6
  | IdAsp_Kind_Con
%%]
%%[[8
  | IdAsp_Val_FFI       {iaspDecl   ::  EH.Decl                         }
%%]
%%[[9
  | IdAsp_Class_Class
  | IdAsp_Class_Def     {iaspDecls  ::  EH.Decl                         }
  | IdAsp_Inst_Inst
  | IdAsp_Inst_Def      {iaspDecl   ::  EH.Decl, iaspClassNm :: HsName  }
  | IdAsp_Dflt_Def      {iaspDecl   ::  EH.Decl                         }
%%]
  | IdAsp_Any
%%]

%%[1 hs
iaspIsSig :: IdAspect -> Bool
iaspIsSig (IdAsp_Val_Sig _) = True
iaspIsSig _                 = False
%%]

%%[1 hs
iaspIsFun :: IdAspect -> Bool
iaspIsFun (IdAsp_Val_Fun _ _) = True
iaspIsFun _                   = False
%%]

%%[1 hs
instance Show IdAspect where
  show _ = "IdAspect"
%%]

%%[1 hs
instance PP IdAspect where
  pp  IdAsp_Val_Var         = pp "VAR"
  pp (IdAsp_Val_Pat _  )    = pp "PAT"
  pp (IdAsp_Val_Fun _ _)    = pp "FUN"
  pp (IdAsp_Val_Sig _  )    = pp "SIG"
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
  pp (IdAsp_Type_Def _ )    = pp "DEF"
%%]
%%[[6
  pp  IdAsp_Kind_Con        = pp "CON"
%%]
%%[[8
  pp (IdAsp_Val_FFI _  )    = pp "FFI"
%%]
%%[[9
  pp  IdAsp_Class_Class     = pp "CLS"
  pp (IdAsp_Class_Def _)    = pp "DEF"
  pp  IdAsp_Inst_Inst       = pp "INS"
  pp (IdAsp_Inst_Def _ _)   = pp "DEF"
  pp (IdAsp_Dflt_Def _ )    = pp "DEF"
%%]
  pp  IdAsp_Any             = pp "ANY"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defining occurrence
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 hs
data IdDefOcc
  = IdDefOcc
      { doccOcc     :: IdOcc
      , doccAsp     :: IdAspect
      , doccLev     :: NmLev
      , doccRange   :: Range
%%[[12
      , doccNmAlts  :: Maybe [HsName]
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

%%[12 -1.mkIdDefOcc hs
mkIdDefOcc :: IdOcc -> IdAspect -> NmLev -> Range -> IdDefOcc
mkIdDefOcc o a l r = IdDefOcc o a l r Nothing
%%]

%%[1
instance PP IdDefOcc where
  pp o = doccOcc o >|< "/" >|< doccAsp o >|< "/" >|< doccLev o
%%[[12
         >|< maybe empty (\ns -> "/" >|< ppBracketsCommas ns) (doccNmAlts o)
%%]
%%]
