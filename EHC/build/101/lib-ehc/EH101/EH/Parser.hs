module EH101.EH.Parser
( pAGItf )
where
import System.IO
import UU.Parsing
import UU.Parsing.Offside
import EH.Util.ParseUtils
import UU.Scanner.GenToken
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Scanner.Common
import EH101.EH
import EH101.Ty
import qualified Data.Set as Set
import EH101.Foreign.Parser
import EH101.Foreign




{-# LINE 30 "src/ehc/EH/Parser.chs" #-}
type EHCParser        ep    =    LayoutParser Token ep

pAGItf                      ::   EHCParser AGItf

pExpr, pExprApp, pExprBase  ::   EHCParser Expr
pExprPrefix                 ::   EHCParser (Expr -> Expr)

pDecls                      ::   EHCParser Decls
pDecl                       ::   EHCParser Decl

pPatExpr, pPatExprBase      ::   EHCParser PatExpr

pTyExpr, pTyExprBase        ::   EHCParser TyExpr

pInt                        ::   EHCParser Int
pChr                        ::   EHCParser Char

pCon                        ::   EHCParser HsName
pVar                        ::   EHCParser HsName

{-# LINE 52 "src/ehc/EH/Parser.chs" #-}
pTyExprPrefix               ::   EHCParser (TyExpr -> TyExpr)

{-# LINE 56 "src/ehc/EH/Parser.chs" #-}
pTyExprApp                  ::   EHCParser TyExpr

{-# LINE 60 "src/ehc/EH/Parser.chs" #-}
pCaseAlts                   ::   EHCParser CaseAlts
pCaseAlt                    ::   EHCParser CaseAlt

pDataConstr                 ::   EHCParser DataConstr
pDataConstrs                ::   EHCParser DataConstrs

pTyVars                     ::   EHCParser TyVars
pTyVar                      ::   EHCParser TyVar

{-# LINE 71 "src/ehc/EH/Parser.chs" #-}
pDataLabFields              ::   EHCParser DataFields
pDataFields                 ::   EHCParser DataFields
pDataLabField               ::   EHCParser DataField
pDataField                  ::   EHCParser DataField

{-# LINE 82 "src/ehc/EH/Parser.chs" #-}
pApp            ::   SemApp ep => EHCParser ep -> EHCParser ep
pApp p          =    mkApp <$> pList1 p

{-# LINE 99 "src/ehc/EH/Parser.chs" #-}
pChr            =    head <$> pChar
pInt            =    read <$> pInteger
pCon            =    hsnFromString <$> pConid
pVar            =    hsnFromString <$> pVarid

{-# LINE 110 "src/ehc/EH/Parser.chs" #-}
pAGItf          =    AGItf_AGItf <$> pExpr

{-# LINE 118 "src/ehc/EH/Parser.chs" #-}
pDecls          =    foldr (:) []         <$>  pBlock pOCurly pSemi pCCurly pDecl
pDecl           =    mkEH Decl_Val        <$>  pPatExprBase  <*   pEQUAL   <*> pExpr
                <|>  mkEH Decl_TySig      <$>  pVar          <*   pDCOLON  <*> pTyExpr
{-# LINE 123 "src/ehc/EH/Parser.chs" #-}
                <|>  (\c tvs cons -> mkEH Decl_Data False c tvs cons Nothing)
                                          <$   pDATA         <*>  pCon       <*> pTyVars
                                                             <*   pEQUAL     <*> pDataConstrs
{-# LINE 132 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH Decl_KiSig      <$>  pCon          <*   pDCOLON    <*> pKiExpr
{-# LINE 135 "src/ehc/EH/Parser.chs" #-}
                <|>  (\(conv,_) saf imp nm sig
                        -> mkEH Decl_FFI conv saf
                             (
                               (\i -> fst $ parseForeignEnt ForeignDirection_Import conv Nothing i)
                               (if null imp then show nm else imp))
                             nm sig
                     )
                     <$   pFOREIGN <* pIMPORT <*> pFFIWay
                     <*>  (pV (   pSAFE
                              <|> pUNSAFE
                              ) `opt` "safe")
                     <*>  (pString `opt` "")
                     <*>  pVar
                     <*   pDCOLON <*> pTyExpr
{-# LINE 155 "src/ehc/EH/Parser.chs" #-}
                <|>  pDeclClass
                <|>  pDeclInstance
{-# LINE 159 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH Decl_Type       <$   pTYPE         <*>  pCon
                                                             <*   pEQUAL     <*> pTyExpr
{-# LINE 172 "src/ehc/EH/Parser.chs" #-}
pPatExprBase    =    pVar <**>  (    flip (mkEH PatExpr_VarAs) <$ pAT <*> pPatExprBase
                                <|>  pSucceed (mkEH PatExpr_Var)
                                )
                <|>  mkEH PatExpr_Con     <$>  pCon
                <|>  mkEH PatExpr_IConst  <$>  pInt
                <|>  mkEH PatExpr_CConst  <$>  pChr
{-# LINE 183 "src/ehc/EH/Parser.chs" #-}
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" Nothing
                        (mkEH RecPatExpr_Empty,mkEH RecPatExpr_Expr . mkEH PatExpr_Var,mkEH RecPatExpr_Ext,mkEH PatExpr_Rec,mkEH PatExpr_Parens)
                        pSel pPatExpr

{-# LINE 189 "src/ehc/EH/Parser.chs" #-}
pPatExpr        =    pApp pPatExprBase
{-# LINE 192 "src/ehc/EH/Parser.chs" #-}
                     <??> (mkEH PatExpr_TypeAs <$ pDCOLON <*> pTyExpr)

{-# LINE 200 "src/ehc/EH/Parser.chs" #-}
pKiExpr, pKiExprBase        ::   EHCParser KiExpr

pKiExprBase     =    mkEH KiExpr_Con <$> (pCon <|> pHNm pSTAR)
                <|>  mkEH KiExpr_Var <$> pVar
                <|>  pParens pKiExpr
pKiExpr         =    pChainr (mk1Arrow <$ pKeyw hsnArrow) pKiExprBase

{-# LINE 213 "src/ehc/EH/Parser.chs" #-}
pTyExprBase     =    mkEH TyExpr_Con       <$>  pCon
{-# LINE 216 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH TyExpr_Wild      <$   pTDOT
{-# LINE 219 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH TyExpr_Var       <$>  pVar
                <|>  mkEH TyExpr_VarWild   <$   pPERCENT <*> pVar
{-# LINE 226 "src/ehc/EH/Parser.chs" #-}
                <|>  pParenRow False (show hsnORow) (show hsnCRow) "::" Nothing
                        (mkEH RowTyExpr_Empty,semVar,mkEH RowTyExpr_Ext,mkEH TyExpr_Row,id)
                        pVar pTyExpr
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "::" Nothing
                        (mkEH RowTyExpr_Empty,semVar,mkEH RowTyExpr_Ext
                            ,\r -> mkConApp hsnRec [mkEH TyExpr_Row r]
                            ,mkEH TyExpr_Parens)
                        pVar pTyExpr
                <|>  pParenRow False (show hsnOSum) (show hsnCSum) "::" Nothing
                        (mkEH RowTyExpr_Empty,semVar,mkEH RowTyExpr_Ext
                            ,\r -> mkConApp hsnSum [mkEH TyExpr_Row r]
                            ,id)
                        pVar pTyExpr
{-# LINE 244 "src/ehc/EH/Parser.chs" #-}
                where  semVar = (mkEH RowTyExpr_Var)

{-# LINE 253 "src/ehc/EH/Parser.chs" #-}
pTyExpr         =    pTyExprPrefix <*> pTyExpr
                <|>  pTyExprApp <??> (flip mk1Arrow <$ pKeyw hsnArrow <*> pTyExpr)

{-# LINE 258 "src/ehc/EH/Parser.chs" #-}
pTyExprs        ::   EHCParser TyExprs
pTyExprs        =    pList pTyExprBase

{-# LINE 263 "src/ehc/EH/Parser.chs" #-}
pTyExprPrefix   =    mkEH TyExpr_Quant
                     <$>  (tyQu_Forall <$ pKey "forall" <|> tyQu_Exists <$ pKey "exists")
                     <*>  pVar <* pKey "."
{-# LINE 268 "src/ehc/EH/Parser.chs" #-}
                <|>  pTyPrExprPrefix
{-# LINE 271 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH TyExpr_Lam <$ pLAM <*> pVar <* pRARROW

{-# LINE 275 "src/ehc/EH/Parser.chs" #-}
pTyPrExprPrefix ::   EHCParser (TyExpr -> TyExpr)
pTyPrExprPrefix =    mk1Arrow
                     <$>  pPackImpl
                            (    pPr <|> pIm
                            <|>  pSucceed  (mkEH TyExpr_NoImpls)
                            )
                     <*   pKeyw hsnArrow
                <|>  (    mk1Arrow <$> (pPrB <|> pIm)
                     <|>  flip (foldr mk1Arrow)
                          <$> pParens ((:) <$> pPr <*> (pImO <|> (++) <$> pList1 (pComma *> pPr) <*> pImO))
                     )
                     <*   pKeyw hsnPrArrow
                where  pPrB  =   mkEH TyExpr_Pred   <$>  pPrExprBase
                       pPr   ::  EHCParser TyExpr
                       pPr   =   mkEH TyExpr_Pred   <$>  pPrExpr
                       pIm   ::  EHCParser TyExpr
                       pIm   =   mkEH TyExpr_Impls  <$   pKey "..."
                       pImO  =   (:[]) <$ pComma <*> pIm `opt` []

{-# LINE 299 "src/ehc/EH/Parser.chs" #-}
pTyExprApp      =    pApp pTyExprBase

{-# LINE 303 "src/ehc/EH/Parser.chs" #-}
pPackImpl       ::   IsParser p Token => p v -> p v
pPackImpl       =    pPacked pOIMPL pCIMPL

{-# LINE 313 "src/ehc/EH/Parser.chs" #-}
pExprBase       =    mkEH Expr_IConst     <$>  pInt
                <|>  mkEH Expr_CConst     <$>  pChr
                <|>  mkEH Expr_Var        <$>  pVar
                <|>  mkEH Expr_Con        <$>  pCon
{-# LINE 322 "src/ehc/EH/Parser.chs" #-}
                <|>  (\e a -> mkEH Expr_Case e a Nothing Set.empty False)
                     <$   pKey "case" <*> pExpr <* pKey "of" <*> pCaseAlts
{-# LINE 330 "src/ehc/EH/Parser.chs" #-}
                <|>  pParenRow True (show hsnORec) (show hsnCRec) "=" (Just (":=",mkEH RecExpr_Upd))
                        (mkEH RecExpr_Empty,mkEH RecExpr_Expr . mkEH Expr_Var,mkEH RecExpr_Ext,mkEH Expr_Rec,semParens)
                        pVar pExpr
{-# LINE 335 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH Expr_Undefined  <$   pKey "..."
{-# LINE 338 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH Expr_DynVar     <$>  pDynVar

{-# LINE 343 "src/ehc/EH/Parser.chs" #-}
pExpr           =    pE <??> (mkEH Expr_TypeAs <$ pKey "::" <*> pTyExpr)
                where pE  =    pExprPrefix <*> pE
                          <|>  pExprApp

{-# LINE 352 "src/ehc/EH/Parser.chs" #-}
pExprApp        =    pE <??> ((\l e -> semAppTop (foldl (flip ($)) e l)) <$> pList1 pA)
{-# LINE 358 "src/ehc/EH/Parser.chs" #-}
                where  pA = flip semApp <$> pE <|> pImpred <|> pImpl
{-# LINE 364 "src/ehc/EH/Parser.chs" #-}
                       pImpred = (flip (mkEH Expr_AppImpred)) <$ pKey "~" <*> pE
{-# LINE 367 "src/ehc/EH/Parser.chs" #-}
                       pE = pExprBase <**> pExprSelSuffix
{-# LINE 370 "src/ehc/EH/Parser.chs" #-}
                       pImpl = pPackImpl ((\a p e -> mkEH Expr_AppImpl e p a) <$> pExpr <* pKey "<:" <*> pPrExpr)

{-# LINE 374 "src/ehc/EH/Parser.chs" #-}
pExprPrefix     =    mkEH Expr_Let <$ pLET
                     <*> pMaybe False (const True) pBANG
                     <*> pDecls    <* pIN
{-# LINE 381 "src/ehc/EH/Parser.chs" #-}
                <|>  (\c t e ->  mkEH Expr_Case c
                                   [ mkEH CaseAlt_Pat (mkEH PatExpr_Con hsnTrue) t
                                   , mkEH CaseAlt_Pat (mkEH PatExpr_Con hsnFalse) e
                                   ]
                                   Nothing Set.empty False
                     )
                     <$ pIF <*> pExpr <* pTHEN <*> pExpr <* pELSE
{-# LINE 400 "src/ehc/EH/Parser.chs" #-}
                <|>  (flip (foldr ($)))
                     <$   pKey "\\"
                     <*>  pList1  (    mkEH Expr_Lam <$> pPatExprBase
                                  <|>  pPackImpl (flip (mkEH Expr_LamImpl) <$> pPatExpr <* pKey "<:" <*> pPrExpr)
                                  )
                     <*   pKey "->"

{-# LINE 420 "src/ehc/EH/Parser.chs" #-}
pDataConstr     =    (\c -> mkEH DataConstr_Constr c Nothing)
                     <$> pCon <*> (pDataFields <|> pCurly pDataLabFields)
                     <*> pSucceed Nothing
{-# LINE 437 "src/ehc/EH/Parser.chs" #-}
pDataConstrs    =    pListSep (pKey "|") pDataConstr

{-# LINE 441 "src/ehc/EH/Parser.chs" #-}
pDataField      =    mkEH DataField_Field Nothing <$> pTyExprBase
pDataLabField   =    mkEH DataField_Field <$> (Just <$> pList1Sep pComma pVar) <* pKey "::" <*> pTyExpr
pDataFields     =    pList pDataField
pDataLabFields  =    pList1Sep pComma pDataLabField

{-# LINE 450 "src/ehc/EH/Parser.chs" #-}
pCaseAlts       =    foldr (:) []
                     <$> pBlock1 pOCurly pSemi pCCurly pCaseAlt
pCaseAlt        =    mkEH CaseAlt_Pat  <$>  pPatExpr <* pKey "->" <*> pExpr

pTyVars         =    pList pTyVar
pTyVar          =    mkEH TyVar_Var <$> pVar

{-# LINE 459 "src/ehc/EH/Parser.chs" #-}
pTyVars1        ::   EHCParser TyVars
pTyVars1        =    pList1 pTyVar

{-# LINE 468 "src/ehc/EH/Parser.chs" #-}
data RowFld a = FldSel HsName a | FldNoSel a | FldUpd HsName a

pParenRow       ::   Bool -> String -> String -> String -> Maybe (String,r -> HsName -> e -> r)
                     -> (r,HsName -> r,r -> Maybe HsName -> e -> r,r -> e,e -> e)
                     -> EHCParser HsName -> EHCParser e -> EHCParser e

pParenRow singleAsIs o c sep mbUpd (semEmpty,semVar,semExt,semRow,semParens) pSel pE
                =    pKey o *> pRowFlds <* pKey c
                where  pFld          =    ((pSel <**> pSep) <|> pSucceed FldNoSel) <*> pE
                       pFlds         =    pListSep pComma pFld
                       pExtFlds      =    mkE <$> (pRowNested <|> semVar <$> pVar) <* pKey "|" <*> pFlds
                       pFldsOrExt    =    mkE semEmpty <$> pFlds <|> pExtFlds
                       pRowNested    =    pKey o *> pFldsOrExt <* pKey c
                       pRowFlds      =    if singleAsIs
                                          then       pFld <**>  (    (\fs f -> mkR (f:fs)) <$ pComma <*> pFlds
                                                                <|>  pSucceed (\le -> case le of {FldNoSel e -> semParens e; _ -> mkR [le]})
                                                                )
                                                <|>  semRow <$> pExtFlds
                                                <|>  pSucceed (mkR [])
                                          else  semRow <$> pFldsOrExt
                       mkR fs        =    semRow (mkE semEmpty fs )
                       mkE ext fs    =    foldl (\r f -> case f of
                                                            FldSel l e -> semExt r (Just l) e
                                                            FldNoSel e -> semExt r Nothing e
                                                            FldUpd l e -> semUpd r l e
                                                ) ext fs
                       (pSep,semUpd) =    case mbUpd of
                                            Just (sepUpd,sem) -> (FldSel <$ pKey sep <|> FldUpd <$ pKey sepUpd,sem)
                                            Nothing           -> (FldSel <$ pKey sep,\r _ _ -> r)

{-# LINE 500 "src/ehc/EH/Parser.chs" #-}
pExprSelSuffix  ::   EHCParser (Expr -> Expr)
pExprSelSuffix  =    (\lbls e -> foldl (mkEH Expr_Sel) e lbls)
                     <$> pList (pHASH *> pSel)

pSel            ::   EHCParser HsName
pSel            =    pVar <|> pCon <|> mkHNmPos <$> pInt

{-# LINE 513 "src/ehc/EH/Parser.chs" #-}
pPrExprClass    ::   EHCParser PrExpr
pPrExprClass    =    mkEH PrExpr_Class  <$> pCon <*> pTyExprs

{-# LINE 518 "src/ehc/EH/Parser.chs" #-}
pPrExprPrefix   ::   EHCParser (PrExpr -> PrExpr)
pPrExprPrefix   =    mkEH PrExpr_Arrow  <$> pPrExprBase <* pKeyw hsnPrArrow
                <|>  mkEH PrExpr_Forall <$  pKey "forall" <*> pVar <* pKey "."

{-# LINE 524 "src/ehc/EH/Parser.chs" #-}
pPrExpr         ::   EHCParser PrExpr
pPrExpr         =    pPrExprPrefix <*> pPrExpr
                <|>  pPrExprBase

{-# LINE 530 "src/ehc/EH/Parser.chs" #-}
pTyPrExpr       ::   EHCParser TyExpr
pTyPrExpr       =    pTyPrExprPrefix <*> pTyPrExpr
                <|>  mkEH TyExpr_Pred <$> pPrExprBase

{-# LINE 536 "src/ehc/EH/Parser.chs" #-}
pPrExprBase     ::   EHCParser PrExpr
pPrExprBase     =    pPrExprClass
                <|>  pParens pPrExpr
{-# LINE 541 "src/ehc/EH/Parser.chs" #-}
                <|>  mkEH PrExpr_DynVar <$> pDynVar <* pKey "::" <*> pTyExpr
                <|>  pVar <**>  (    (\s v -> mkEH PrExpr_Lacks (mkEH RowTyExpr_Var v) s)
                                     <$ pKey "\\" <*> pSel
{-# LINE 550 "src/ehc/EH/Parser.chs" #-}
                                )

{-# LINE 558 "src/ehc/EH/Parser.chs" #-}
pClassHead      ::   EHCParser TyExpr
pClassHead      =    pTyPrExprPrefix <*> pHd <|> pHd
                where pHd = mkEH TyExpr_Pred <$> pPrExprClass

pDeclClass      ::   EHCParser Decl
pDeclClass      =    (\h deps d -> mkEH Decl_Class h deps Nothing d [])
                     <$   pKey "class"
                     <*>  pClassHead
                     <*>  (pKey "|" *> pListSep pComma (mkEH FuncDep_Dep <$> pTyVars1 <* pKey "->" <*> pTyVars1)
                          `opt` []
                          )
                     <*   pKey "where" <*> pDecls

pDeclInstance   ::   EHCParser Decl
pDeclInstance   =    pKey "instance"
                     *>   (    (\n h d -> mkEH Decl_Instance n InstNormal h d)
                               <$>  ((\n e -> Just (n,e)) <$> pVar <*> (True <$ pKey "<:" <|> False <$ pKey "::") `opt` Nothing)
                               <*>  pClassHead
                               <*   pKey "where" <*> pDecls
                          <|>  mkEH Decl_InstanceIntro Nothing <$> pExpr <* pKey "<:" <*> pPrExprClass
                          )

{-# LINE 594 "src/ehc/EH/Parser.chs" #-}
pDynVar         ::   EHCParser HsName
pDynVar         =    pKeyw hsnDynVar *> pVar

