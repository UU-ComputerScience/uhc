{-| Module      :  ParseLibrary
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.ParseLibrary where 

import Text.ParserCombinators.Parsec hiding (satisfy)
import Text.ParserCombinators.Parsec.Pos(newPos)
import Helium.Parser.Lexer
import Helium.Syntax.UHA(Name(..), Range(..), Position(..))
import Helium.Syntax.UHA_Utils
import qualified Helium.Utils.Texts as Texts

type HParser a = GenParser Token SourcePos a

runHParser :: HParser a -> FilePath -> [Token] -> Bool -> Either ParseError a
runHParser p fname tokens withEOF =
    runParser 
        (if withEOF then waitForEOF p else p) 
        (newPos fname 0 0) 
        fname 
        tokens

waitForEOF p
  = do{ x <- p
      ; lexeme LexEOF
      ; return x
      }

tycls   = name   lexCon  <?> Texts.parserTypeClass
tycon   = name   lexCon  <?> Texts.parserTypeConstructor
tyvar   = name   lexVar  <?> Texts.parserTypeVariable
modid   = name   lexCon  <?> Texts.parserModuleName
varid   = name   lexVar  <?> Texts.parserVariable
conid   = name   lexCon  <?> Texts.parserVariable
consym  = opName lexConSym
       <?> Texts.parserOperator
varsym  = opName (   lexVarSym 
                 <|> do { lexMIN;    return "-" } 
                 <|> do { lexMINDOT; return "-." }
                 )
       <?> Texts.parserOperator

-- var  ->  varid | ( varsym )  (variable)  
var = varid <|> parens varsym
   <?> Texts.parserVariable

-- con  ->  conid | ( consym )  (constructor)  
con = conid <|> parens consym
   <?> Texts.parserVariable

-- op  ->  varop | conop  (operator)  
-- expanded for better parse errors
op = varsym <|> consym <|> lexBACKQUOTEs (varid <|> conid) 
  <?> Texts.parserOperator

-- varop  ->  varsym | `varid ` (variable operator)  
varop = varsym <|> lexBACKQUOTEs varid
     <?> Texts.parserOperator
        
-- conop  ->  consym | `conid ` (constructor operator)  
conop = consym <|> lexBACKQUOTEs conid
     <?> Texts.parserOperator

name :: HParser String -> HParser Name
name p = addRange $
    do 
        n <- p
        return (\r -> Name_Identifier r [] n) -- !!!Name

opName :: HParser String -> HParser Name
opName p = addRange $
    do 
        n <- p
        return (\r -> Name_Operator r [] n) -- !!!Name

addRange :: HParser (Range -> a) -> HParser a
addRange p =
    do 
        start <- getPosition
        f <- p
        end <- getLastPosition
        let r = Range_Range (sourcePosToPosition start) (sourcePosToPosition end)
        return (f r)

withRange :: HParser a -> HParser (a, Range)
withRange p = addRange (do { x <- p; return (\r -> (x, r)); })

sourcePosToPosition sourcePos =
    Position_Position 
        (sourceName sourcePos) 
        (sourceLine sourcePos)
        (sourceColumn sourcePos)

lexBACKQUOTEs p = between lexBACKQUOTE lexBACKQUOTE p
brackets   p = between lexLBRACKET  lexRBRACKET  p

commas  p = p `sepBy`  lexCOMMA
commas1 p = p `sepBy1` lexCOMMA

lexINSERTED_SEMI     = lexeme LexInsertedSemicolon
lexINSERTED_LBRACE   = lexeme LexInsertedOpenBrace
lexINSERTED_RBRACE   = lexeme LexInsertedCloseBrace

lexLBRACE   = lexeme (LexSpecial '{')
lexRBRACE   = lexeme (LexSpecial '}')
lexLPAREN   = lexeme (LexSpecial '(')
lexRPAREN   = lexeme (LexSpecial ')')
lexLBRACKET = lexeme (LexSpecial '[')
lexRBRACKET = lexeme (LexSpecial ']')
lexCOMMA    = lexeme (LexSpecial ',')
lexSEMI     = lexeme (LexSpecial ';')
lexBACKQUOTE = lexeme (LexSpecial '`')

lexASG      = lexeme (LexResVarSym "=")
lexLARROW   = lexeme (LexResVarSym "<-")
lexRARROW   = lexeme (LexResVarSym "->")
lexDARROW   = lexeme (LexResVarSym "=>")
lexBAR      = lexeme (LexResVarSym "|")
lexMIN      = lexeme (LexResVarSym "-")
lexMINDOT   = lexeme (LexResVarSym "-.")
lexBSLASH   = lexeme (LexResVarSym "\\")
lexAT       = lexeme (LexResVarSym "@")
lexDOTDOT   = lexeme (LexResVarSym "..")
lexTILDE    = lexeme (LexResVarSym "~")

lexCOLCOL   = lexeme (LexResConSym "::")

lexCLASS    = lexeme (LexKeyword "class")
lexDATA     = lexeme (LexKeyword "data")
lexDERIVING = lexeme (LexKeyword "deriving")
lexTYPE     = lexeme (LexKeyword "type")
lexLET      = lexeme (LexKeyword "let")
lexIN       = lexeme (LexKeyword "in")
lexDO       = lexeme (LexKeyword "do")
lexIF       = lexeme (LexKeyword "if")
lexTHEN     = lexeme (LexKeyword "then")
lexELSE     = lexeme (LexKeyword "else")
lexCASE     = lexeme (LexKeyword "case")
lexOF       = lexeme (LexKeyword "of")
lexMODULE   = lexeme (LexKeyword "module")
lexWHERE    = lexeme (LexKeyword "where")
lexIMPORT   = lexeme (LexKeyword "import")
lexHIDING   = lexeme (LexKeyword "hiding")
lexINFIX    = lexeme (LexKeyword "infix")
lexINFIXL   = lexeme (LexKeyword "infixl")
lexINFIXR   = lexeme (LexKeyword "infixr")
lexUNDERSCORE = lexeme (LexKeyword "_")


-- Typing strategies
lexPHASE       = lexeme (LexKeyword "phase")
lexCONSTRAINTS = lexeme (LexKeyword "constraints")
lexSIBLINGS    = lexeme (LexKeyword "siblings")
lexCOL         = lexeme (LexResConSym ":")
lexASGASG      = lexeme (LexResVarSym "==")

{-
Expressions and patterns with operators are represented by lists. The Range
of this list is 'noRange' (a range with two unknown positions). The post-processor 
recognises this and will build infix applications out of the list.
The list itself contains expressions (/patterns) and operators. Operators can
be recognised because they also have noRange. Their name, however, does have a range.
The unary minus has 'unaryMinus' as its name to distinguish it from the binary minus.

An example,  "-3+4" is parsed as:

Expression_List <<unknown>,<unknown>> 
    [   Expression_Variable <<unknown>,<unknown>> (Name_Identifier <<1,1>,<1,2>> [] "unaryMinus")
    ,   Expression_Literal <<1,2>,<1,3>> (Literal_Int <<1,2>,<1,3>> "3")
    ,   Expression_Variable <<unknown>,<unknown>> (Name_Identifier <<1,3>,<1,4>> [] "+")
    ,   Expression_Literal <<1,4>,<1,5>> (Literal_Int <<1,4>,<1,5>> "4")
    ]

-}

----------------------------------------------------------------
-- Extra parser combinators
----------------------------------------------------------------

withLayout p =
    withBraces (semiSepTerm p) (semiOrInsertedSemiSepTerm p)

withLayout1 p =
    withBraces (semiSepTerm1 p) (semiOrInsertedSemiSepTerm1 p)

withBraces' p = 
    withBraces (p True) (p False)

withBraces p1 p2 =
    do
        lexLBRACE
        x <- p1
        lexRBRACE
        return x
    <|>
    do 
        lexINSERTED_LBRACE
        x <- p2
        lexINSERTED_RBRACE
        return x
    
semiSepTerm1 p = p `sepEndBy1` lexSEMI
semiSepTerm  p = p `sepEndBy`  lexSEMI

semiOrInsertedSemiSepTerm1 p = p `sepEndBy1` (lexINSERTED_SEMI <|> lexSEMI)
semiOrInsertedSemiSepTerm  p = p `sepEndBy`  (lexINSERTED_SEMI <|> lexSEMI)

parens  p     = between lexLPAREN lexRPAREN p
braces  p     = between lexLBRACE lexRBRACE p

----------------------------------------------------------------
-- Basic parsers
----------------------------------------------------------------

lexeme :: Lexeme -> HParser ()
lexeme lex
  = satisfy (\lex' -> if (lex == lex') then Just () else Nothing) <?> show lex


lexChar :: HParser String
lexChar
  = satisfy (\lex -> case lex of { LexChar c -> Just c; other -> Nothing })

lexString :: HParser String
lexString
  = satisfy (\lex -> case lex of { LexString s -> Just s; other -> Nothing })

lexDouble :: HParser String
lexDouble
  = satisfy (\lex -> case lex of { LexFloat d -> Just d; other -> Nothing })

lexInt :: HParser String
lexInt
  = satisfy (\lex -> case lex of { LexInt i -> Just i; other -> Nothing })

lexVar :: HParser String
lexVar
  = satisfy (\lex -> case lex of { LexVar s -> Just s; other -> Nothing })
                          
lexCon :: HParser String
lexCon
  = satisfy (\lex -> case lex of { LexCon s -> Just s; other -> Nothing })

lexVarSym :: HParser String
lexVarSym
  = satisfy (\lex -> case lex of { LexVarSym s -> Just s; other -> Nothing })

lexConSym :: HParser String
lexConSym
  = satisfy (\lex -> case lex of { LexConSym s -> Just s; other -> Nothing })


satisfy :: (Lexeme -> Maybe a) -> HParser a
satisfy pred
  = tokenPrimEx
        showtok 
        nextpos 
        (Just (\oldpos (pos,lex) lexemes old -> incSourceColumn pos (lexemeLength lex)))
        (\(pos,lex) -> pred lex)
  where
    showtok (pos,lex)   = show lex
    nextpos _ _ ((pos,lex):_)
       = pos
    nextpos pos _ []
       = pos

getLastPosition :: HParser SourcePos
getLastPosition = getState
