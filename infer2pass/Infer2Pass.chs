%%[1 import(IO, Data.List, UU.Pretty, UU.Parsing, UU.Scanner, UU.Scanner.Position (initPos,Pos), MainAG)
%%]

-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

%%[1
main :: IO ()
main
  = do { txt <- hGetContents stdin
       ; let tokens = scan [ "let", "in"
%%]
%%[2
                           , "forall", "Char", "Int"
%%]
%%[1
                           ]
                           [ "->", "=", "\\"
%%]
%%[2
                           , "::", "."
%%]
%%[1
                           ]
                           "()" ":\\->=." (initPos "") txt
       ; pres <- parseIOMessage show pAGItf tokens
       ; let res = wrap_AGItf pres Inh_AGItf
       ; putStrLn (disp (pp_Syn_AGItf res) 200 "")
       }
%%]

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

%%[1
pAGItf :: (IsParser p Token) => p T_AGItf
pAGItf
  = pAGItf
  where pAGItf    =   sem_AGItf_AGItf <$> pExpr
        pExprBase =   pParens pExpr
                  <|> sem_Expr_Var           <$> pVarid
                  <|> (sem_Expr_Int . read)  <$> pInteger
%%]
%%[2
                  <|> (sem_Expr_Char . head) <$> pChar
%%]
%%[1
        pExprApp  =   foldl1 sem_Expr_App    <$> pList1 pExprBase
        pExprPre  =   sem_Expr_Lam
                      <$ pKey "\\"  <*> pVarid
                      <* pKey "->"
%%]
%%[1.let
                  <|> sem_Expr_Let
                      <$ pKey "let" <*> pVarid
                      <* pKey "="   <*> pExpr <* pKey "in"
%%]
%%[2.let -1.let
                  <|> pKey "let" *> pVarid
                      <**> (   (\te e i -> sem_Expr_TLet i te e) <$ pKey "::" <*> pTyExpr <* pKey "=" <*> pExpr <* pKey "in"
                           <|> (\e i -> sem_Expr_Let i e) <$ pKey "=" <*> pExpr <* pKey "in"
                           )
%%]
%%[1
        pExpr     =   pExprPre <*> pExpr
                  <|> pExprApp
%%]
%%[2
        pTyExprPre=   sem_TyExpr_Arr
                      <$> pTyExprBase
                      <*  pKey "->"
                  <|> sem_TyExpr_All
                      <$  pKey "forall" <*> pVarid
                      <*  pKey "."
        pTyExpr   =   pTyExprPre <*> pTyExpr
                  <|> pTyExprBase
        pTyExprBase
                  =   pParens pTyExpr
                  <|> sem_TyExpr_Var        <$> pVarid
                  <|> sem_TyExpr_Char       <$  pKey "Char"
                  <|> sem_TyExpr_Int        <$  pKey "Int"
%%]