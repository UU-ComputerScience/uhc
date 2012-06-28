module EH101.NameAspect
( IdDefOcc (..), emptyIdDefOcc, mkIdDefOcc
, IdAspect (..)
, iaspIsFun, iaspIsPat, iaspIsValSig, iaspIsValVar, iaspIsValFix
, iaspIsTypeVar
, iaspIsValCon, iaspIsTypeDef, iaspIsValFld
, iaspIsTypeSig
, iaspIsValForeign
, iaspIsClassDef
, doccStrip )
where
import EH101.Base.Common
import EH101.Base.Builtin
import qualified EH101.EH as EH
import EH.Util.Pretty

{-# LINE 55 "src/ehc/NameAspect.chs" #-}
data IdAspect
  = IdAsp_Val_Var
  | IdAsp_Val_Pat       	{iaspDecl   ::  EH.Decl                         }
  | IdAsp_Val_Fun       	{iaspPatL   :: [EH.PatExpr], iaspBody :: EH.Expr, iaspUniq :: !UID}
  | IdAsp_Val_Sig       	{iaspDecl   ::  EH.Decl                         }
  | IdAsp_Val_Fix
  | IdAsp_Val_Con
  | IdAsp_Val_Fld       	{iaspDataNm :: !HsName, iaspConNm :: !HsName    }
  | IdAsp_Val_Fusion	  	{iaspDecl   ::  EH.Decl                         }
  | IdAsp_Fusion_Conv  		{iaspDecl   ::  EH.Decl                         }
  | IdAsp_Type_Con
  | IdAsp_Type_Var
  | IdAsp_Type_Def      	{iaspDecl   ::  EH.Decl                         }
  | IdAsp_Type_Sig      	{iaspDecl   :: !EH.Decl                         }
  | IdAsp_Kind_Con
  | IdAsp_Kind_Var
  | IdAsp_Val_Foreign   	{iaspDecl   :: !EH.Decl                         }
  | IdAsp_Class_Class
  | IdAsp_Class_Def     	{iaspDecl   ::  EH.Decl, iaspDeclInst ::  EH.Decl}
  | IdAsp_Inst_Inst
  | IdAsp_Inst_Def      	{iaspDecl   ::  EH.Decl, iaspClassNm  :: !HsName}
  | IdAsp_Dflt_Def     		-- for now defaults without explicit class name are ignored
                       	 	{iaspDecl   :: !EH.Decl, iaspIgnore   :: !Bool  }
  | IdAsp_Any

{-# LINE 96 "src/ehc/NameAspect.chs" #-}
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

{-# LINE 118 "src/ehc/NameAspect.chs" #-}
iaspIsTypeVar :: IdAspect -> Bool
iaspIsTypeVar IdAsp_Type_Var = True
iaspIsTypeVar _              = False

{-# LINE 124 "src/ehc/NameAspect.chs" #-}
iaspIsValCon :: IdAspect -> Bool
iaspIsValCon IdAsp_Val_Con = True
iaspIsValCon _             = False

iaspIsValFld :: IdAspect -> Bool
iaspIsValFld (IdAsp_Val_Fld _ _) = True
iaspIsValFld _                   = False

iaspIsTypeDef :: IdAspect -> Bool
iaspIsTypeDef (IdAsp_Type_Def _) = True
iaspIsTypeDef _                  = False

{-# LINE 138 "src/ehc/NameAspect.chs" #-}
iaspIsTypeSig :: IdAspect -> Bool
iaspIsTypeSig (IdAsp_Type_Sig _) = True
iaspIsTypeSig _                  = False

{-# LINE 144 "src/ehc/NameAspect.chs" #-}
iaspIsValForeign :: IdAspect -> Bool
iaspIsValForeign (IdAsp_Val_Foreign _) = True
iaspIsValForeign _                     = False

{-# LINE 150 "src/ehc/NameAspect.chs" #-}
iaspIsClassDef :: IdAspect -> Bool
iaspIsClassDef (IdAsp_Class_Def _ _) = True
iaspIsClassDef _                     = False

{-# LINE 156 "src/ehc/NameAspect.chs" #-}
instance Show IdAspect where
  show _ = "IdAspect"

{-# LINE 161 "src/ehc/NameAspect.chs" #-}
instance PP IdAspect where
  pp  IdAsp_Val_Var         = pp "value"
  pp (IdAsp_Val_Pat _    )  = pp "pattern"
  pp (IdAsp_Val_Fun _ _ _)  = pp "function"
  pp (IdAsp_Val_Sig _    )  = pp "type signature"
  pp  IdAsp_Val_Fix         = pp "fixity"
  pp  IdAsp_Val_Con         = pp "data constructor"
  pp (IdAsp_Val_Fld _ _  )  = pp "data field"
  pp (IdAsp_Val_Fusion  _)  = pp "fuse"
  pp (IdAsp_Fusion_Conv _)  = pp "convert"
  pp  IdAsp_Type_Con        = pp "type constructor"
  pp  IdAsp_Type_Var        = pp "type variable"
  pp (IdAsp_Type_Def _   )  = pp "type"
  pp (IdAsp_Type_Sig _   )  = pp "kind signature"
  pp  IdAsp_Kind_Con        = pp "kind constructor"
  pp  IdAsp_Kind_Var        = pp "kind variable"
  pp (IdAsp_Val_Foreign _)  = pp "foreign"
  pp  IdAsp_Class_Class     = pp "class"
  pp (IdAsp_Class_Def _ _)  = pp "class"
  pp  IdAsp_Inst_Inst       = pp "instance"
  pp (IdAsp_Inst_Def  _ _)  = pp "instance"
  pp (IdAsp_Dflt_Def  _ _)  = pp "default"
  pp  IdAsp_Any             = pp "ANY"

{-# LINE 205 "src/ehc/NameAspect.chs" #-}
data IdDefOcc
  = IdDefOcc
      { doccOcc     :: !IdOcc
      , doccAsp     :: !IdAspect
      , doccLev     :: !NmLev
      , doccRange   :: !Range
      , doccNmAlts  :: !(Maybe [HsName])
      }
  deriving (Show)

emptyIdDefOcc :: IdDefOcc
emptyIdDefOcc = mkIdDefOcc (IdOcc hsnUnknown IdOcc_Any) IdAsp_Any nmLevAbsent emptyRange

{-# LINE 227 "src/ehc/NameAspect.chs" #-}
mkIdDefOcc :: IdOcc -> IdAspect -> NmLev -> Range -> IdDefOcc
mkIdDefOcc o a l r = IdDefOcc o a l r Nothing

{-# LINE 232 "src/ehc/NameAspect.chs" #-}
instance PP IdDefOcc where
  pp o = doccOcc o >|< "/" >|< doccAsp o >|< "/" >|< doccLev o
         >|< maybe empty (\ns -> "/" >|< ppBracketsCommas ns) (doccNmAlts o)

{-# LINE 240 "src/ehc/NameAspect.chs" #-}
-- | Strip positional info
doccStrip :: IdDefOcc -> IdDefOcc
doccStrip o = o {doccRange = emptyRange}

{-# LINE 297 "src/ehc/NameAspect.chs" #-}
instance PP IdOcc where
  pp o = ppCurlysCommas [pp (ioccNm o),pp (ioccKind o)]

