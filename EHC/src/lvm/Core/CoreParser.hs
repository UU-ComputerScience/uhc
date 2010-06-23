{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreParser.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.CoreParser( parseModule ) where

import Lvm.Common.IdSet( setFromList )
import Text.ParserCombinators.Parsec
import Lvm.Core.CoreLex

import Lvm.Common.Byte( bytesFromString )
import Lvm.Common.Id
import Lvm.Core.Core

-----------------------------------------------------------
-- 
-----------------------------------------------------------   
parseModule :: FilePath -> IO CoreModule
parseModule fname
  = do{ input <- readFile fname
      ; case runParser pmodule () fname input of
          Left err  -> ioError (userError ("parse error: " ++ show err))
          Right mod -> return mod
      }


-----------------------------------------------------------
-- Module 
-----------------------------------------------------------   
pmodule :: Parser CoreModule
pmodule
  = topLevel $
    do{ reserved "module"
      ; name <- modid
      ; reserved "where"
      ; declss <- ptopdecl `termBy` semi <|> curlies (ptopdecl `termBy` semi)
      ; return (Module name 0 0 (concat declss))
      }

ptopdecl :: Parser [CoreDecl]
ptopdecl
  = pvaluedecl

-----------------------------------------------------------
-- Value declarations
-----------------------------------------------------------   
pvaluedecl :: Parser [CoreDecl]
pvaluedecl
  = do{ optional (reserved "val")
      ; name <- varid
      ; args <- many bindid
      ; special "="
      ; expr <- pexpr
      ; let body = foldr Lam expr args
      ; return [DeclValue name defaultAccess Nothing body []]
      }

-----------------------------------------------------------
-- Expressions
-----------------------------------------------------------   
pexpr :: Parser Expr
pexpr 
  = do{ special "\\"
      ; args <- many1 bindid
      ; special "->"
      ; body <- pexpr
      ; return (foldr Lam body args)
      }
  <|>
    do{ reserved "let"
      ; makelet <- plet
      ; binds   <- pdecls
      ; reserved "in"
      ; body    <- pexpr
      ; return (makelet binds body)
      }
  <|> fexpr
  <?> "expression"

pdecls :: Parser [Bind]
pdecls
  = pdecl `sepTermBy1` semi

pdecl :: Parser Bind
pdecl
  = do{ name <- varid
      ; args <- many bindid
      ; special "="
      ; free <- option [] (curlies (many varid))
      ; expr <- pexpr
      ; let body = if (null free) then expr else Note (FreeVar (setFromList free)) expr
      ; return (Bind name (foldr Lam body args))
      }

plet :: Parser ([Bind] -> Expr -> Expr)
plet
  = do{ reserved "val"
      ; return (\binds body -> foldr (\bind x -> Let (NonRec bind) x) body binds)
      }
  <|>
    do{ special "!"
      ; return (\binds body -> foldr (\bind x -> Let (Strict bind) x) body binds)
      }
  <|>
    do{ optional (reserved "rec")
      ; return (\binds body -> Let (Rec binds) body)
      }

fexpr
  = do{ xs <- many1 aexpr
      ; return (foldl1 Ap xs)
      }

aexpr
  = var <|> con <|> literal <|> parens pexpr
  <?> "atomic expression"

literal
  = do{ x <- integerOrFloat 
      ; case x of Left i  -> return (Lit (LitInt (fromInteger i)))
                  Right f -> return (Lit (LitDouble f))
      }
  <|>
    do{ special "-"
      ; x <- integerOrFloat
      ; case x of Left i  -> return (Lit (LitInt (fromInteger (negate i))))
                  Right f -> return (Lit (LitDouble (negate f)))
      } 
  <|> 
    do{ s <- stringLiteral
      ; return (Lit (LitBytes (bytesFromString s)))
      }

-----------------------------------------------------------
-- 
-----------------------------------------------------------   

-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------   
var 
  = do{ name <- varid
      ; return (Var name)
      }

con 
  = do{ name <- conid
      ; return (Con (ConId name))
      }
  <|>
    do{ special "#"
      ; lparen
      ; tag <- pexpr
      ; comma
      ; arity <- integer
      ; rparen
      ; return (Con (ConTag tag (fromInteger arity)))
      }

modid,bindid :: Parser Id
bindid  = varid
modid   = conid

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------   
parens p  = do{ lparen; x <- p; rparen; return x }
curlies p = do{ lcurly; x <- p; rcurly; return x }

comma     = special "," 
semi      = special ";"
lparen    = special "("
rparen    = special ")"
lcurly    = special "{"
rcurly    = special "}"

sepTermBy1 p sep
  = sepEndBy1 p sep

termBy p sep
  = many (do{ x <- p; sep; return x })

defaultAccess :: Access
defaultAccess
  = private