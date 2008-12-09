%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94 module {%{EH}Foreign.Parser} import(UU.Scanner.GenToken, {%{EH}Base.Builtin},{%{EH}Base.Common}, {%{EH}Scanner.Common}, {%{EH}Foreign})
%%]

%%[94 import(EH.Util.ParseUtils, UU.Parsing)
%%]

%%[94 import({%{EH}Error},{%{EH}Error.Pretty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Parse to ForeignEnt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[94 hs export(parseForeignEnt)
parseForeignEnt :: String -> (ForeignEnt,ErrL)
parseForeignEnt s
  = (res,errs)
  where tokens     = scan foreignEntScanOpts (initPos s) s
        (res,msgs) = parseToResMsgs pForeignEnt tokens
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
pForeignEnt :: ForeignParser ForeignEnt
pForeignEnt
  = ForeignEnt_CCall <$> pCCall

pCCall :: ForeignParser CCall
pCCall
  =   CCall_Id
        <$> pMaybe False (const True) pSTATIC
        <*> pMaybe Nothing (\v -> Just (v ++ ".h")) (pForeignVar <* pDOT <* pH)
        <*> pMaybe False (const True) pAMPERSAND
        <*> pForeignVar
  <|> CCall_Dynamic <$ pDYNAMIC
  <|> CCall_Wrapper <$ pWRAPPER
  <|> pSucceed CCall_Empty

pForeignVar :: ForeignParser String
pForeignVar = tokGetVal <$> (pVARID <|> pCONID)
%%]

