{------------------------------------------------------------------------
  The Core Assembler.

  Copyright 2001, Daan Leijen. All rights reserved. This file
  is distributed under the terms of the GHC license. For more
  information, see the file "license.txt", which is included in
  the distribution.
------------------------------------------------------------------------}

--  $Id: CoreLex.hs 222 2004-02-14 16:33:04Z uust $

module Lvm.Core.CoreLex( topLevel
              , varid, conid, anyid
              , reserved, special
              , integerOrFloat, integer, stringLiteral 
              ) where

import Char  ( digitToInt, isAlphaNum, isLower, isUpper )
import Lvm.Common.Set   ( Set, fromList, member )
import Lvm.Common.Id    ( Id, idFromString )

import Text.ParserCombinators.Parsec hiding (space,tab,lower,upper,alphaNum)


----------------------------------------------------------
-- Testing
-----------------------------------------------------------   
testOk fname
  = testEx ("c:\\daan\\runtime\\test\\correct\\corelexer\\" ++ fname ++ ".cor")

testErr fname
  = testEx ("c:\\daan\\runtime\\test\\error\\corelexer\\" ++ fname ++ ".cor")

testEx fname
  = do{ result <- parseFromFile ptokens fname
      ; case result of
          Left err -> do{ putStr "parse error: "; print err }
          Right xs -> putStr (unlines xs)
      }

ptokens :: Parser [String]
ptokens
  = topLevel (many token)
  where
    token =   do{ x <- integerOrFloat
                ; case x of Left i  -> return (show i)
                            Right f -> return (show f) 
                }
          <|> showit stringLiteral
          <|> showit varid
          <|> showit conid
     
    showit p  = do{ x <- p; return (show x) }
          
  
-----------------------------------------------------------
-- Reserved
-----------------------------------------------------------   
special :: String -> Parser ()
special name
  = lexeme (skip (string name) <?> name)

reserved :: String -> Parser ()
reserved name 
  = lexeme $ try (
    do{ string name
      ; notFollowedBy idchar <?> ("end of " ++ show name)
      }  
    <?> name) 

isReserved :: String -> Bool
isReserved name
  = Set.member name reservedNames

reservedNames :: Set String
reservedNames
  = Set.fromList $
    [ "module", "where"
    , "import", "abstract", "extern"
    , "custom", "val", "con"
    , "match", "with"
    , "let", "rec", "in"
    , "static", "dynamic", "runtime"
    , "stdcall", "ccall", "instruction"
    , "decorate"
    , "private", "public", "nothing"
    , "type", "data", "forall", "exist"
    , "case", "of"
    , "if", "then", "else"
    ]


-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------
integerOrFloat :: Parser (Either Integer Double)
integerOrFloat  = lexeme (intOrFloat) <?> "number"

float           = lexeme floating   <?> "float"
integer         = lexeme int        <?> "integer"


-- floats
floating        = do{ n <- decimal 
                    ; fractExponent n
                    }


intOrFloat      = do{ char '0'
                    ; zeroNumFloat
                    }
                  <|> decimalFloat
                  
zeroNumFloat    =  do{ n <- hexadecimal <|> octal
                     ; return (Left n)
                     }
                <|> decimalFloat
                <|> fractFloat 0
                <|> return (Left 0)                  
                  
decimalFloat    = do{ n <- decimal
                    ; option (Left n) 
                             (fractFloat n)
                    }

fractFloat n    = do{ f <- fractExponent n
                    ; return (Right f)
                    }
                    
fractExponent n = do{ fract <- try fraction -- "try" due to ".." as in "[1..6]"
                    ; expo  <- option 1.0 exponent'
                    ; return ((fromInteger n + fract)*expo)
                    }
                <|>
                  do{ expo <- exponent'
                    ; return ((fromInteger n)*expo)
                    }

fraction        = do{ char '.'
                    ; digits <- many1 digit <?> "fraction"
                    ; return (foldr op 0.0 digits)
                    }
                  <?> "fraction"
                where
                  op d f    = (f + fromIntegral (digitToInt d))/10.0
                    
exponent'       = do{ oneOf "eE"
                    ; f <- sign
                    ; e <- decimal <?> "exponent"
                    ; return (power (f e))
                    }
                  <?> "exponent"
                where
                   power e  | e < 0      = 1.0/power(-e)
                            | otherwise  = fromInteger (10^e)

sign            =   (char '-' >> return negate) 
                <|> (char '+' >> return id)     
                <|> return id



-- integers
int             = zeroNumber <|> decimal
    
zeroNumber      = do{ char '0'
                    ; hexadecimal <|> octal <|> decimal <|> return 0
                    }
                  <?> ""       

decimal         = number 10 digit        
hexadecimal     = do{ oneOf "xX"; number 16 hexDigit }
octal           = do{ oneOf "oO"; number 8 octDigit  }

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }


-----------------------------------------------------------
-- Identifiers
-----------------------------------------------------------   
anyid :: Parser (Either Id Id)
anyid
  =   do{ x <- varid; return (Left x) }
  <|> do{ x <- conid; return (Right x) }

varid,conid :: Parser Id
varid  
  = lexeme (
    do{ name <- lowerid <|> extid '$'
      ; return (idFromString name)
      } 
    <?> "variable")

conid  
  = lexeme (
    do{ name <- upperid <|> extid '@' 
      ; return (idFromString name)
      }
    <?> "constructor")


upperid :: Parser String
upperid
  = do{ c  <- upper
      ; cs <- many idchar
      ; return (c:cs)
      }

lowerid :: Parser String
lowerid
  = try $
    do{ c  <- lower
      ; cs <- many idchar
      ; let name = c:cs
      ; if (isReserved name)
         then unexpected ("reserved word " ++ show name)
         else return name
      }

idchar :: Parser Char
idchar
  = alphanum <|> oneOf "_'"


-- extended identifiers
extid :: Char -> Parser String
extid start
  = do{ char start
      ; xs <- many extchar
      ; return (foldr (maybe id (:)) "" xs)
      }

extchar :: Parser (Maybe Char)
extchar
  =   do{ c <- extletter; return (Just c) }
  <|> extescape
  <?> "identifier character"

extletter
  = satisfy (\c -> isGraphic c && not (elem c "\\."))

extescape
  = do{ char '\\'
      ;     do{ escapeempty; return Nothing }
        <|> do{ esc <- escape; return (Just esc) }
      }

-----------------------------------------------------------
-- Strings
-----------------------------------------------------------
stringLiteral :: Parser String
stringLiteral   = lexeme (
                  do{ str <- between (char '"')                   
                                     (char '"' <?> "end of string")
                                     (many stringchar) 
                    ; return (foldr (maybe id (:)) "" str)
                    }
                  <?> "string")

stringchar :: Parser (Maybe Char)
stringchar      =   do{ c <- stringletter; return (Just c) }
                <|> stringescape 
                <?> "string character"
            
stringletter    = satisfy (\c -> c==' ' || (isGraphic c && not (elem c "\"\\")))

stringescape    = do{ char '\\'
                    ;     do{ escapegap  ; return Nothing }
                      <|> do{ escapeempty; return Nothing }
                      <|> do{ esc <- escape; return (Just esc) }
                    }
                    
escapeempty     = char '&'
escapegap       = do{ whitespace
                    ; char '\\' <?> "end of string gap"
                    }
                                       
escape          = charesc <|> charnum <?> "escape code"

charnum :: Parser Char
charnum         = do{ code <- decimal 
                              <|> do{ char 'o'; number 8 octDigit }
                              <|> do{ char 'x'; number 16 hexDigit }
                    ; return (toEnum (fromInteger code))
                    }

charesc         = choice (map parseEsc escMap)
                where
                  parseEsc (c,code) = do{ char c; return code }
                  escMap            = zip ("abfnrstv\\\"\'.") 
                                          ("\a\b\f\n\r \t\v\\\"\'.")


-----------------------------------------------------------
-- Lexeme
-----------------------------------------------------------   
lexeme :: Parser a -> Parser a
lexeme p
  = do{ x <- p
      ; whitespace
      ; return x
      }

-----------------------------------------------------------
-- Whitespace
-----------------------------------------------------------   
topLevel p
  = do{ whitespace 
      ; x <- p
      ; eof
      ; return x
      }

whitespace :: Parser ()
whitespace 
  = skipMany (skip white <|> linecomment <|> blockcomment <?> "")
                                               
linecomment 
  = do{ try (string "--")
      ; skipMany linechar
      }

linechar
  = graphic <|> space <|> tab

blockcomment 
  = do{ try (string "{-")
      ; incomment
      }

incomment 
    =   do{ try (string "-}");        return () }
    <|> do{ blockcomment;             incomment }
    <|> do{ skipMany1 contentchar;    incomment }
    <|> do{ skip (oneOf commentchar); incomment }
    <?> "end of comment"  
    where
      commentchar     = "-{}"
      contentchar     = white <|> satisfy (\c -> isGraphic c && not (elem c commentchar))


-----------------------------------------------------------
-- Character classes
-----------------------------------------------------------   
white    = oneOf " \n\r\t"
space    = char ' '
tab      = char '\t'
alphanum = satisfy isAlphaNum
lower    = satisfy isLower
upper    = satisfy isUpper
graphic  = satisfy isGraphic

isGraphic c
  =  (code >= 0x21   && code <= 0xD7FF) || (code >= 0xE000 && code <= 0xFFFD)
  where
    code = fromEnum c

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------   
skip :: Parser a -> Parser ()
skip p 
  = do{ p; return () }