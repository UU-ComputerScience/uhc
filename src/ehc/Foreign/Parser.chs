%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Parse a foreign entity.
Parsing is parameterized with:

\begin{itemize}
\item The calling convention (encoded by @FFIWay@), determines which parser is used as the syntax depends on the calling convention.

\item A name, to be used when a name is absent. This feature is to be used for a first parse only, after that the name should be explicitly specified.
This is to cater for @ccall@ calling convention which allows (e.g.) an include file only (taken from library module @Foreign.Marshall.Utils@):
\begin{pre}
foreign import ccall unsafe "string.h" memcpy  :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
\end{pre}

\end{itemize}

%%]

%%[90 module {%{EH}Foreign.Parser} import(UU.Scanner.GenToken, {%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Foreign})
%%]

%%[90 import(EH.Util.ParseUtils, UU.Parsing, EH.Util.Utils)
%%]

%%[90 import({%{EH}Error},{%{EH}Error.Pretty})
%%]

%%[90 import({%{EH}Base.Target})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parse to ForeignEnt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[90 hs export(parseForeignEnt)
parseForeignEnt :: ForeignDirection -> FFIWay -> Maybe String -> String -> (ForeignEnt,ErrL)
parseForeignEnt dir way dfltNm s
  = (res,errs)
  where tokens     = scan (foreignEntScanOpts way) (initPos s) s
        (res,msgs) = parseToResMsgs (pForeignEnt dir way dfltNm) tokens
        errs       = map (rngLift emptyRange mkPPErr) msgs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for foreign entities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These parsers are only used by the HS frontend to parse the string holding the import/export entity of a foreign function
to a corresponding abstract syntax.

%%[90
type ForeignParser        ep    =    PlainParser Token ep
%%]

%%[90
pForeignEnt :: ForeignDirection -> FFIWay -> Maybe String -> ForeignParser ForeignEnt
pForeignEnt dir way dfltNm
  = case (dir,way) of
      (_                      ,FFIWay_CCall  ) -> ForeignEnt_CCall        <$> pCCall       dfltNm
      (_                      ,FFIWay_Prim   ) -> ForeignEnt_PrimCall     <$> pPrimCall    dfltNm
%%[[(90 jscript)
      (ForeignDirection_Import,FFIWay_JScript) -> ForeignEnt_JScriptCall  <$> pJScriptCall dfltNm
%%]]
      _                                        -> ForeignEnt_PlainCall    <$> pPlainCall   dfltNm

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

pJScriptCall :: Maybe String -> ForeignParser JScriptCall
pJScriptCall dfltNm
  =   JScriptCall_Id nm <$> pMb pForeignExpr
  <|> JScriptCall_Dynamic <$ pDYNAMIC
  <|> JScriptCall_Wrapper <$ pWRAPPER
  where nm = maybe "" id dfltNm

pForeignVar :: ForeignParser String
pForeignVar = tokGetVal <$> (pVARID <|> pCONID)

pForeignExpr :: ForeignParser ForeignExpr
pForeignExpr
  = pExp
  where pExp  = pObj <|> mk <$> pPre <*> pExpB <*> pPost
        pPre  = pList pPtr
        pPost = pList (pSel <|> pInx <|> pCall)
        pExpB = pArg <|> pEnt
        pArg  = pPERCENT
                <**> (   const ForeignExpr_AllArg               <$  pSTAR
                     <|> (\i _ -> ForeignExpr_Arg (tokMkInt i)) <$> pInteger10Tk
                     )
              <|> pStr
        pObj  = ForeignExpr_ObjData <$ pOCURLY <* pCCURLY
        pEnt  = ForeignExpr_EntNm <$> pForeignVar
        pStr  = (ForeignExpr_Str . tokMkStr) <$> pStringTk
        pPtr  = ForeignExpr_Ptr <$ pAMPERSAND
        pInx  = (flip ForeignExpr_Inx) <$ pOBRACK <*> pExp <* pCBRACK
        pSel  = (flip ForeignExpr_Sel) <$ pDOT <*> pEnt
        pCall = flip ForeignExpr_CallArgs <$ pOPAREN <*> pListSep pCOMMA pArg <* pCPAREN
        mk    = \pre e post -> foldr ($) e $ pre ++ reverse post
%%]

