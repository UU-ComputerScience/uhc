module EH101.Foreign.Parser
( parseForeignEnt )
where
import UU.Scanner.GenToken
import EH101.Base.Builtin
import EH101.Base.Common
import EH101.Scanner.Common
import EH101.Foreign
import EH.Util.ParseUtils
import UU.Parsing
import EH.Util.Utils
import EH101.Error
import EH101.Error.Pretty
import EH101.Base.Target

{-# LINE 44 "src/ehc/Foreign/Parser.chs" #-}
parseForeignEnt :: ForeignDirection -> FFIWay -> Maybe String -> String -> (ForeignEnt,ErrL)
parseForeignEnt dir way dfltNm s
  = (res,errs)
  where tokens     = scan (foreignEntScanOpts way) (initPos s) s
        (res,msgs) = parseToResMsgs (pForeignEnt dir way dfltNm) tokens
        errs       = map (rngLift emptyRange mkPPErr) msgs

{-# LINE 60 "src/ehc/Foreign/Parser.chs" #-}
type ForeignParser        ep    =    PlainParser Token ep

{-# LINE 64 "src/ehc/Foreign/Parser.chs" #-}
pForeignEnt :: ForeignDirection -> FFIWay -> Maybe String -> ForeignParser ForeignEnt
pForeignEnt dir way dfltNm
  = case (dir,way) of
      (_                      ,FFIWay_CCall  		) -> ForeignEnt_CCall        	<$> pCCall       dfltNm
      (_                      ,FFIWay_Prim   		) -> ForeignEnt_PrimCall     	<$> pPrimCall    dfltNm
      (ForeignDirection_Import,FFIWay_JavaScript	) -> ForeignEnt_JavaScriptCall  <$> pJavaScriptCall dfltNm
      _                                        		  -> ForeignEnt_PlainCall    	<$> pPlainCall   dfltNm

pCCall :: Maybe String -> ForeignParser CCall
pCCall dfltNm
  =   (True <$ pSTATIC) <**> pAfterStatic
  <|> ($ False) <$> pAfterStatic
  <|> CCall_Dynamic <$ pDYNAMIC
  <|> CCall_Wrapper <$ pWRAPPER
  where nm = maybe "" id dfltNm
        pPtrForeignVar
          = pAMPERSAND
            <**> (     const <$> pForeignVar
                 `opt` (const nm)
                 )
        pAfterStatic
          = pForeignVar
                <**> (     (pDOT <* pH)
                           <**> (     (\nm _ incl st -> CCall_Id st (mkincl incl) True  nm) <$> pPtrForeignVar
                                <|>   (\nm _ incl st -> CCall_Id st (mkincl incl) False nm) <$> pForeignVar
                                `opt` (\_ incl st -> CCall_Id st (mkincl incl) False nm)
                                )
                     `opt` (\nm st -> CCall_Id st Nothing False nm)
                     )
          <|>   (\nm st -> CCall_Id st Nothing True nm) <$> pPtrForeignVar
          `opt` (\st -> CCall_Id st Nothing False nm)
          where mkincl i = Just (i ++ ".h")

pPlainCall :: Maybe String -> ForeignParser PlainCall
pPlainCall dfltNm
  =     PlainCall_Id <$> pForeignVar
  `opt` PlainCall_Id nm
  where nm = maybe "" id dfltNm

pPrimCall :: Maybe String -> ForeignParser PrimCall
pPrimCall dfltNm
  = PrimCall_Id <$> (pForeignVar `opt` nm) <*> pKnownPrim
  where nm = maybe "" id dfltNm
        pKnownPrim = pMb (pAnyFromMap pKeyTk allKnownPrimMp)

pJavaScriptCall :: Maybe String -> ForeignParser JavaScriptCall
pJavaScriptCall dfltNm
  =   JavaScriptCall_Id nm   <$> pMb pForeignExpr
  <|> JavaScriptCall_Dynamic <$  pDYNAMIC
  <|> JavaScriptCall_Wrapper <$  pWRAPPER
  where nm = maybe "" id dfltNm

pForeignVar :: ForeignParser String
pForeignVar = tokGetVal <$> (pVARID <|> pCONID)

pForeignExpr :: ForeignParser ForeignExpr
pForeignExpr
  = pExp
  where pExp      = pObj <|> mk <$> pPre <*> pExpB <*> pPost
        pPre      = pMb (ForeignExpr_NewObj <$ pNEW)
        pPost     = pList (pSel <|> pInx <|> pCall)
        pExpB     = pArg <|> pEnt
        pManyArg  = ForeignExpr_AllArg <$ pPERCENT <* pSTAR
        pArg      = (pPERCENT *> ((ForeignExpr_Arg . tokMkInt) <$> pInteger10Tk)) <|> pStr
        pObj      = ForeignExpr_ObjData <$ pOCURLY <* pCCURLY
        pEnt      = ForeignExpr_EntNm <$> pForeignVar
        pStr      = (ForeignExpr_Str . tokMkStr) <$> pStringTk
        pInx      = flip ForeignExpr_Inx <$ pOBRACK <*> pExp <* pCBRACK
        pSel      = flip ForeignExpr_Sel <$ pDOT <*> (pEnt <|> pArg)
        pCall     = flip ForeignExpr_CallArgs <$ pOPAREN <*> pCallExpr <* pCPAREN
        pCallExpr = ((\x -> [x]) <$> pManyArg) <|> (pListSep pCOMMA pExpB)

        mk    = \pre e post -> let pre' = maybe [] ((flip (:)) []) pre
                               in foldr ($) e $ pre' ++ reverse post


