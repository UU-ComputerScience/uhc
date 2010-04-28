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

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EH for an id
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Not used

%%[1 hs export(IdEH(..))
data IdEH
  = IdEH_Val_Pat       	{iehDecl   ::  EH.Decl                         }
  | IdEH_Val_Fun       	{iehPatL   :: [EH.PatExpr], iehBody :: EH.Expr, iehUniq :: !UID}
  | IdEH_Val_Sig       	{iehDecl   ::  EH.Decl                         }
%%[[5
  | IdEH_Type_Def      	{iehDecl   :: !EH.Decl                         }
%%]]
%%[[6
  | IdEH_Type_Sig      	{iehDecl   :: !EH.Decl                         }
%%]]
%%[[8
  | IdEH_Val_Foreign   	{iehDecl   :: !EH.Decl                         }
%%]]
%%[[9
  | IdEH_Class_Class
  | IdEH_Class_Def     	{iehDecl   :: !EH.Decl, iehDeclInst :: !EH.Decl}
  | IdEH_Inst_Def      	{iehDecl   ::  EH.Decl, iehClassNm  :: !HsName}
  | IdEH_Dflt_Def      	{iehDecl   :: !EH.Decl                         }
%%]]
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
  | IdAsp_Val_Fld       {iaspDataNm :: !HsName, iaspConNm :: !HsName    }
%%]]
  | IdAsp_Type_Con
%%[[3
  | IdAsp_Type_Var
%%]]
%%[[5
  | IdAsp_Type_Def      {iaspDecl   ::  EH.Decl                         }
%%]]
%%[[6
  | IdAsp_Type_Sig      {iaspDecl   :: !EH.Decl                         }
  | IdAsp_Kind_Con
  | IdAsp_Kind_Var
%%]]
%%[[8
  | IdAsp_Val_Foreign   {iaspDecl   :: !EH.Decl                         }
%%]]
%%[[9
  | IdAsp_Class_Class
  | IdAsp_Class_Def     {iaspDecl   :: !EH.Decl, iaspDeclInst :: !EH.Decl}
  | IdAsp_Inst_Inst
  | IdAsp_Inst_Def      {iaspDecl   ::  EH.Decl, iaspClassNm  :: !HsName}
  | IdAsp_Dflt_Def      -- for now defaults are ignored
                        -- {iaspDecl   :: !EH.Decl                         }
%%]]
  | IdAsp_Any
%%]

%%[1 hs export(iaspIsFun,iaspIsPat,iaspIsValSig,iaspIsValVar,iaspIsValFix)
iaspIsFun :: IdAspect -> Bool
iaspIsFun (IdAsp_Val_Fun _ _ _) = True
iaspIsFun _                     = False

iaspIsPat :: IdAspect -> Bool
iaspIsPat (IdAsp_Val_Pat _) = True
iaspIsPat _                 = False

iaspIsValSig :: IdAspect -> Bool
iaspIsValSig (IdAsp_Val_Sig _) = True
iaspIsValSig _                 = False

iaspIsValVar :: IdAspect -> Bool
iaspIsValVar IdAsp_Val_Var = True
iaspIsValVar _             = False

iaspIsValFix :: IdAspect -> Bool
iaspIsValFix IdAsp_Val_Fix = True
iaspIsValFix _             = False
%%]

%%[3 hs export(iaspIsTypeVar)
iaspIsTypeVar :: IdAspect -> Bool
iaspIsTypeVar IdAsp_Type_Var = True
iaspIsTypeVar _              = False
%%]

%%[5 hs export(iaspIsValCon,iaspIsTypeDef,iaspIsValFld)
iaspIsValCon :: IdAspect -> Bool
iaspIsValCon IdAsp_Val_Con = True
iaspIsValCon _             = False

iaspIsValFld :: IdAspect -> Bool
iaspIsValFld (IdAsp_Val_Fld _ _) = True
iaspIsValFld _                   = False

iaspIsTypeDef :: IdAspect -> Bool
iaspIsTypeDef (IdAsp_Type_Def _) = True
iaspIsTypeDef _                  = False
%%]

%%[6 hs export(iaspIsTypeSig)
iaspIsTypeSig :: IdAspect -> Bool
iaspIsTypeSig (IdAsp_Type_Sig _) = True
iaspIsTypeSig _                  = False
%%]

%%[8 hs export(iaspIsValForeign)
iaspIsValForeign :: IdAspect -> Bool
iaspIsValForeign (IdAsp_Val_Foreign _) = True
iaspIsValForeign _                     = False
%%]

%%[9 hs export(iaspIsClassDef)
iaspIsClassDef :: IdAspect -> Bool
iaspIsClassDef (IdAsp_Class_Def _ _) = True
iaspIsClassDef _                     = False
%%]

%%[1 hs
instance Show IdAspect where
  show _ = "IdAspect"
%%]

%%[1 hs
instance PP IdAspect where
  pp  IdAsp_Val_Var         = pp "value"
  pp (IdAsp_Val_Pat _    )  = pp "pattern"
  pp (IdAsp_Val_Fun _ _ _)  = pp "function"
  pp (IdAsp_Val_Sig _    )  = pp "type signature"
  pp  IdAsp_Val_Fix         = pp "fixity"
  pp  IdAsp_Val_Con         = pp "data constructor"
%%[[5
  pp (IdAsp_Val_Fld _ _  )  = pp "data field"
%%]]
  pp  IdAsp_Type_Con        = pp "type constructor"
%%[[3
  pp  IdAsp_Type_Var        = pp "type variable"
%%]]
%%[[5
  pp (IdAsp_Type_Def _   )  = pp "type"
%%]]
%%[[6
  pp (IdAsp_Type_Sig _   )  = pp "kind signature"
  pp  IdAsp_Kind_Con        = pp "kind constructor"
  pp  IdAsp_Kind_Var        = pp "kind variable"
%%]]
%%[[8
  pp (IdAsp_Val_Foreign _)  = pp "foreign"
%%]]
%%[[9
  pp  IdAsp_Class_Class     = pp "class"
  pp (IdAsp_Class_Def _ _)  = pp "class"
  pp  IdAsp_Inst_Inst       = pp "instance"
  pp (IdAsp_Inst_Def  _ _)  = pp "instance"
  pp (IdAsp_Dflt_Def     )  = pp "default"
%%]]
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
%%]]
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
%%% Defining occurrence additional aspects
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not used

%%[1 hs export(IdDefAsp(..))
data IdDefAsp
  = IdDefAsp
      { daspOcc     :: !IdOcc
      , daspEH      :: !IdEH
      }
%%]

%%[1 hs export(mkIdDefAsp)
mkIdDefAsp :: IdOcc -> IdEH -> IdDefAsp
mkIdDefAsp o a = IdDefAsp o a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9999
instance ForceEval IdAspect where
  forceEval x@(IdAsp_Val_Pat d    ) | d `seq` True = x
  forceEval x@(IdAsp_Val_Fun p d i) | p `seq` d `seq` forceEval i `seq` True = x
  forceEval x@(IdAsp_Val_Sig d    ) | d `seq` True = x
  forceEval x@(IdAsp_Inst_Def d n ) | d `seq` True = x
  forceEval x                       = x
%%[[102
  fevCount x | x `seq` True = cm1 "IdAspect_*"
%%]]

instance ForceEval IdDefOcc where
  forceEval x@(IdDefOcc o a l r as) | forceEval a `seq` forceEval as `seq` forceEval r `seq` True = x
%%[[102
  fevCount (IdDefOcc o a l r as) = cmUnions [cm1 "IdDefOcc",fevCount o,fevCount a,fevCount as,fevCount r,fevCount l]
%%]]

instance ForceEval IdDefAsp where
  forceEval x@(IdDefAsp o a) = x
%%[[102
  fevCount (IdDefAsp o a) = cmUnions [cm1 "IdDefAsp",fevCount o {-,fevCount a -}]
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

