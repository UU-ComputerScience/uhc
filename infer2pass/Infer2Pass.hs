module Main
  where

import IO
import Data.List
import UU.Pretty
import UU.Parsing
import UU.Scanner
import UU.Scanner.Position (initPos,Pos)
import Utils
import MainAG

-------------------------------------------------------------------------
-- Main
-------------------------------------------------------------------------

main :: IO ()
main
  = do { txt <- hGetContents stdin
       ; let tokens = scan ["let", "in"] ["->", "=", "\\"]
                           "()" "\\->=" (initPos "") txt
       ; pres <- parseIOMessage show pAGItf tokens
       ; let res = wrap_AGItf pres Inh_AGItf
       ; putStrLn (disp (pp_Syn_AGItf res) 200 "")
       }

-------------------------------------------------------------------------
-- Parser
-------------------------------------------------------------------------

pAGItf :: (IsParser p Token) => p T_AGItf
pAGItf
  = pAGItf
  where pAGItf    =   sem_AGItf_AGItf <$> pExpr
        pExprBase =   pParens pExpr
                  <|> sem_Expr_Var          <$> pVarid
                  <|> (sem_Expr_Int . read) <$> pInteger
        pExprApp  =   foldl1 sem_Expr_App   <$> pList1 pExprBase
        pExprPre  =   sem_Expr_Let
                      <$ pKey "let" <*> pVarid
                      <* pKey "="   <*> pExpr <* pKey "in"
                  <|> sem_Expr_Lam
                      <$ pKey "\\"  <*> pVarid
                      <* pKey "->"
        pExpr     =   pExprPre <*> pExpr
                  <|> pExprApp
