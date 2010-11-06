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
  =
{- this def would be nice, but is ambiguous:
      CCall_Id
        <$> pMaybe False (const True) pSTATIC
        <*> pMaybe Nothing (\v -> Just (v ++ ".h")) (pForeignVar <* pDOT <* pH)
        <*> pMaybe False (const True) pAMPERSAND
        <*> pMaybe nm id pForeignVar
-}
{-
      pMaybe False (const True) pSTATIC
      <**> (   pForeignVar
               <**> (   (pDOT <* pH)
                        <**> (   (\nm _ incl st -> CCall_Id st (Just (incl ++ ".h")) True  nm) <$> pPtrForeignVar
                             <|> (\nm _ incl st -> CCall_Id st (Just (incl ++ ".h")) False nm) <$> pForeignVar
                             )
                    <|> pSucceed (\nm st -> CCall_Id st Nothing False nm)
                    )
           <|> (\nm st -> CCall_Id st Nothing True nm) <$> pPtrForeignVar
           <|> pSucceed (\st -> CCall_Id st Nothing False nm)
           )
-}
      (True <$ pSTATIC) <**> pAfterStatic
  <|> ($ False) <$> pAfterStatic
  <|> CCall_Dynamic <$ pDYNAMIC
  <|> CCall_Wrapper <$ pWRAPPER
  -- <|> pSucceed CCall_Empty
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
{-
-}
  = JScriptCall_Id nm <$> pMb pForeignExpr
  where nm = maybe "" id dfltNm
{-
  = mk <$> pAsPtr <*> pThisArgNr <*> pMb pForeignVar <*> pIndexArgNr
  where nm = maybe "" id dfltNm
        pThisArgNr  = pMb (tokMkInt <$ pPERCENT <*> pInteger10Tk <* pDOT)
        pIndexArgNr = pMb (tokMkInt <$ pOBRACK <* pPERCENT <*> pInteger10Tk <* pCBRACK)
        pAsPtr      = pMaybe False (const True) pAMPERSAND
        mk p t@Nothing Nothing  i@Nothing = JScriptCall_Id p t nm i
        mk p t         Nothing  i         = JScriptCall_Id p t "" i
        mk p t         (Just n) i         = JScriptCall_Id p t n  i
-}
{-
pJScriptCall2 :: Maybe String -> ForeignParser JScriptCall
pJScriptCall2 dfltNm
  =   Left  <$> pMaybe nm id pForeignVar
  <|> RIght <$> pForeignExpr
  where nm = maybe "" id dfltNm
-}

pForeignVar :: ForeignParser String
pForeignVar = tokGetVal <$> (pVARID <|> pCONID)

pForeignExpr :: ForeignParser ForeignExpr
pForeignExpr
  = pExp
  where pExp  = mk <$> pPre <*> pExpB <*> pPost
        pPre  = pList (pPtr)
        pPost = pList (pSel <|> pInx <|> pCall)
        pExpB = pArg <|> pEnt
        pArg  = (ForeignExpr_Arg . tokMkInt) <$ pPERCENT <*> pInteger10Tk
        pEnt  = ForeignExpr_EntNm <$> pForeignVar
        pPtr  = ForeignExpr_Ptr <$ pAMPERSAND
        pInx  = (flip ForeignExpr_Inx) <$ pOBRACK <*> pExp <* pCBRACK
        pSel  = (flip ForeignExpr_Sel) <$ pDOT <*> pEnt
        pCall = ForeignExpr_Call <$ pOPAREN <* pCPAREN
        mk    = \pre e post -> foldr ($) e $ pre ++ reverse post
%%]

pCCall :: Maybe String -> ForeignParser CCall
pCCall dfltNm
  =   CCall_Id
        <$> pMaybe False (const True) pSTATIC
        <*> pMaybe Nothing (\v -> Just (v ++ ".h")) (pForeignVar <* pDOT <* pH)
        <*> pMaybe False (const True) pAMPERSAND
        <*> pForeignVar
  <|> CCall_Dynamic <$ pDYNAMIC
  <|> CCall_Wrapper <$ pWRAPPER
  <|> pSucceed CCall_Empty


pCCall :: Maybe String -> ForeignParser CCall
pCCall dfltNm
  =
{- this def would be nice, but is ambiguous:
      CCall_Id
        <$> pMaybe False (const True) pSTATIC
        <*> pMaybe Nothing (\v -> Just (v ++ ".h")) (pForeignVar <* pDOT <* pH)
        <*> pMaybe False (const True) pAMPERSAND
        <*> pMaybe nm id pForeignVar
-}
{-
      pMaybe False (const True) pSTATIC
      <**> (   pForeignVar
               <**> (   (pDOT <* pH)
                        <**> (   (\nm _ incl st -> CCall_Id st (Just (incl ++ ".h")) True  nm) <$> pPtrForeignVar
                             <|> (\nm _ incl st -> CCall_Id st (Just (incl ++ ".h")) False nm) <$> pForeignVar
                             )
                    <|> pSucceed (\nm st -> CCall_Id st Nothing False nm)
                    )
           <|> (\nm st -> CCall_Id st Nothing True nm) <$> pPtrForeignVar
           <|> pSucceed (\st -> CCall_Id st Nothing False nm)
           )
-}
      pSTATIC
      <**> (
           <|>
           )
  <|> CCall_Dynamic <$ pDYNAMIC
  <|> CCall_Wrapper <$ pWRAPPER
  <|> pSucceed CCall_Empty
  where nm = maybe "" id dfltNm
        pPtrForeignVar
          = pAMPERSAND
            <**> (   const <$> pForeignVar
                 <|> pSucceed (const nm)
                 )
        pAfterStatic
          = pForeignVar
              <**> (   (pDOT <* pH)
                       <**> (   (\nm _ incl st -> CCall_Id st (Just (incl ++ ".h")) True  nm) <$> pPtrForeignVar
                            <|> (\nm _ incl st -> CCall_Id st (Just (incl ++ ".h")) False nm) <$> pForeignVar
                            )
                   <|> pSucceed (\nm st -> CCall_Id st Nothing False nm)
                   )
          <|> (\nm st -> CCall_Id st Nothing True nm) <$> pPtrForeignVar
          <|> pSucceed (\st -> CCall_Id st Nothing False nm)
