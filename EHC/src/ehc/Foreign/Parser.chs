%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
parseForeignEnt :: FFIWay -> String -> (ForeignEnt,ErrL)
parseForeignEnt way s
  = (res,errs)
  where tokens     = scan foreignEntScanOpts (initPos s) s
        (res,msgs) = parseToResMsgs (pForeignEnt way) tokens
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
pForeignEnt :: FFIWay -> ForeignParser ForeignEnt
pForeignEnt way
  = case way of
      FFIWay_CCall -> ForeignEnt_CCall <$> pCCall
      _            -> ForeignEnt_PlainCall <$> pPlainCall

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

pPlainCall :: ForeignParser PlainCall
pPlainCall
  =   PlainCall_Id
        <$> pForeignVar

pForeignVar :: ForeignParser String
pForeignVar = tokGetVal <$> (pVARID <|> pCONID)
%%]

