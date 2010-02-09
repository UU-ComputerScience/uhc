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

%%[94 module {%{EH}Foreign.Parser} import(UU.Scanner.GenToken, {%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Foreign})
%%]

%%[94 import(EH.Util.ParseUtils, UU.Parsing, EH.Util.Utils)
%%]

%%[94 import({%{EH}Error},{%{EH}Error.Pretty})
%%]

%%[94 import({%{EH}Base.Target})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parse to ForeignEnt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94 hs export(parseForeignEnt)
parseForeignEnt :: FFIWay -> Maybe String -> String -> (ForeignEnt,ErrL)
parseForeignEnt way dfltNm s
  = (res,errs)
  where tokens     = scan foreignEntScanOpts (initPos s) s
        (res,msgs) = parseToResMsgs (pForeignEnt way dfltNm) tokens
        errs       = map (rngLift emptyRange mkPPErr) msgs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parser for foreign entities
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

These parsers are only used by the HS frontend to parse the string holding the import/export entity of a foreign function
to a corresponding abstract syntax.

%%[94
type ForeignParser        ep    =    PlainParser Token ep
%%]

%%[94 export(pForeignEnt)
pForeignEnt :: FFIWay -> Maybe String -> ForeignParser ForeignEnt
pForeignEnt way dfltNm
  = case way of
      FFIWay_CCall -> ForeignEnt_CCall <$> pCCall dfltNm
      _            -> ForeignEnt_PlainCall <$> pPlainCall dfltNm

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

pForeignVar :: ForeignParser String
pForeignVar = tokGetVal <$> (pVARID <|> pCONID)
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
