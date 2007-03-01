{-| Module      :  Lexer
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.Lexer
    ( lexer, strategiesLexer
    , Token, Lexeme(..)
    , lexemeLength
    , module Helium.Parser.LexerMessage
    ) where

import Helium.Parser.LexerMonad
import Helium.Parser.LexerMessage
import Helium.Parser.LexerToken
import Text.ParserCombinators.Parsec.Pos
import Helium.Utils.Utils(internalError)

import Monad(when)
import Char(ord)
import List(isPrefixOf)

lexer :: String -> [Char] -> Either LexerError ([Token], [LexerWarning])
lexer fileName input = runLexerMonad fileName (mainLexer input)

strategiesLexer :: String -> [Char] -> Either LexerError ([Token], [LexerWarning])
strategiesLexer fileName input = 
    case lexer fileName input of
        Left err -> Left err
        Right (tokens, warnings) -> Right (reserveStrategyNames tokens, warnings)
        
type Lexer = [Char] -> LexerMonad [Token]

mainLexer :: Lexer
mainLexer [] = do
    checkBracketsAtEOF
    pos <- getPos
    return [(incSourceLine (setSourceColumn pos 0) 1, LexEOF)]

mainLexer ('-':'-':cs) 
    | not (nextCharSatisfy isSymbol rest) = do
        incPos (2 + length minuses)
        lexOneLineComment rest
    where
        (minuses, rest) = span (== '-') cs
        
mainLexer ('{':'-':cs) = do 
    pos <- getPos 
    incPos 2
    lexMultiLineComment [pos] 0 cs 
        
mainLexer input@('\'':_) = 
    lexChar input

mainLexer input@('"':_) = 
    lexString input

-- warn if we see something like ".2"
mainLexer ('.':c:cs) 
    | myIsDigit c = do
        pos <- getPos
        lexerWarning (LooksLikeFloatNoDigits (takeWhile myIsDigit (c:cs))) pos
        returnToken (LexVarSym ".") 1 mainLexer (c:cs)
        
mainLexer input@(c:cs) 
    | myIsLower c || c == '_' = -- variable or keyword
        lexName isLetter LexVar LexKeyword keywords input
    | myIsSpace c = do
        when (c == '\t') $ do
            pos <- getPos
            lexerWarning TabCharacter pos
        nextPos c 
        mainLexer cs        
    | myIsUpper c = -- constructor
        lexName isLetter LexCon (internalError "Lexer" "mainLexer" "constructor") [] input
    | c == ':' = -- constructor operator
        lexName isSymbol LexConSym LexResConSym reservedConSyms input
    | isSymbol c = -- variable operator
        lexName isSymbol LexVarSym LexResVarSym reservedVarSyms input
    | c `elem` "([{" = do
        openBracket c
        returnToken (LexSpecial c) 1 mainLexer cs
    | c `elem` ")]}" = do
        closeBracket c
        returnToken (LexSpecial c) 1 mainLexer cs
    | c `elem` specialsWithoutBrackets = 
        returnToken (LexSpecial c) 1 mainLexer cs
    | myIsDigit c = -- number
        lexIntFloat input
    | otherwise = do
        pos <- getPos
        lexerError (UnexpectedChar c) pos

lexName :: (Char -> Bool) -> (String -> Lexeme) -> 
                (String -> Lexeme) -> [String] -> Lexer
lexName predicate normal reserved reserveds cs = do
    let (name@(first:_), rest) = span predicate cs
        lexeme = if name `elem` reserveds
                 then reserved name 
                 else normal   name
    when ((isSymbol first || first == ':') && name `contains` "--") $ do
        pos <- getPos
        lexerWarning CommentOperator pos
    returnToken lexeme (length name) mainLexer rest

contains :: Eq a => [a] -> [a] -> Bool
[] `contains` _ = False
xs@(_:rest) `contains` ys = ys `isPrefixOf` xs || rest `contains` ys

-----------------------------------------------------------
-- Numbers
-----------------------------------------------------------

lexIntFloat :: Lexer
lexIntFloat input = do
    startPos <- getPos
    let (digits, rest) = span myIsDigit input
    case rest of
        ('.':rest2@(next:_)) 
            | myIsDigit next -> do
                let (fraction, rest3) = span myIsDigit rest2
                    prefix = digits ++ "." ++ fraction
                lexMaybeExponent prefix LexFloat rest3
            | next /= '.' -> do
                pos <- getPos
                lexerWarning (LooksLikeFloatNoFraction digits) pos
                lexMaybeExponent digits LexInt rest
        _ ->
            lexMaybeExponent digits LexInt rest

lexMaybeExponent :: String -> (String -> Lexeme) -> Lexer
lexMaybeExponent prefix lexemeFun input = 
    case input of
        ('e':'+':rest) ->
            lexExponent (prefix ++ "e+") rest
        ('E':'+':rest) ->
            lexExponent (prefix ++ "E+") rest
        ('e':'-':rest) ->
            lexExponent (prefix ++ "e-") rest
        ('E':'-':rest) ->
            lexExponent (prefix ++ "E-") rest
        ('e':rest) ->
            lexExponent (prefix ++ "e") rest
        ('E':rest) ->
            lexExponent (prefix ++ "E") rest
        _ -> do
            returnToken (lexemeFun prefix) (length prefix) mainLexer input

lexExponent :: String -> Lexer
lexExponent prefix input = do
    startPos <- getPos
    let posAtExponent = addPos (length prefix) startPos
        (exponentDigits, rest) = span myIsDigit input
    if null exponentDigits then
        lexerError MissingExponentDigits posAtExponent
     else do
        let text = prefix ++ exponentDigits
        returnToken (LexFloat text) (length text) mainLexer rest

-----------------------------------------------------------
-- Characters
-----------------------------------------------------------

lexChar :: Lexer
lexChar input = do 
    pos <- getPos
    case input of
        '\'':'\\':c:'\'':cs -> -- '\n' etc
            if c `elem` escapeChars then    
                returnToken (LexChar ['\\',c]) 4 mainLexer cs
            else
                lexerError IllegalEscapeInChar pos
        '\'':'\'':_ -> -- ''
            lexerError EmptyChar pos
        '\'':c:'\'':cs -> -- 'a' '%'
            if ord c >= 32 && ord c <= 126 then 
                returnToken (LexChar [c]) 3 mainLexer cs
            else
                lexerError IllegalCharInChar pos
        ['\''] -> -- ' at end of file
            lexerError EOFInChar pos
        ('\'':cs) -> -- if there is a name between single quotes, we give a hint that backquotes might be meant 
            let (ds, es) = span (/= '\'') cs
                ws = words ds
            in if length es > 0 && head es == '\'' && length ws == 1 && isName (head ws) then
                    lexerError (NonTerminatedChar (Just (head ws))) pos
               else 
                    lexerError (NonTerminatedChar Nothing) pos
        _ -> internalError "Lexer" "lexChar" "unexpected characters"

lexString :: Lexer
lexString ('"':cs) = 
    lexStringChar "" cs
lexString _ = 
    internalError "Lexer" "lexString" "should start with \""

lexStringChar :: String -> Lexer
lexStringChar revSoFar input = do
    startPos <- getPos
    let curPos = addPos (length revSoFar + 1) startPos
    case input of
        [] -> 
            lexerError EOFInString startPos
        '\\':c:cs ->
            if c `elem` escapeChars then do
                lexStringChar (c:'\\':revSoFar) cs
            else
                lexerError IllegalEscapeInString curPos
        '"':cs ->
            returnToken (LexString (reverse revSoFar)) 
                        (length revSoFar + 2) mainLexer cs
        '\n':_ ->
            lexerError NewLineInString startPos
        c:cs ->
            if ord c >= 32 && ord c <= 126 then
                lexStringChar (c:revSoFar) cs
            else
                lexerError IllegalCharInString curPos
                
nextCharSatisfy :: (Char -> Bool) -> String -> Bool
nextCharSatisfy _ []    = False
nextCharSatisfy p (c:_) = p c

returnToken :: Lexeme -> Int -> Lexer -> Lexer
returnToken lexeme width continue input = do
    pos <- getPos
    incPos width
    let token = (pos, lexeme) 
    tokens <- continue input
    return (token:tokens)
       
-----------------------------------------------------------
-- Comment
-----------------------------------------------------------

lexOneLineComment :: Lexer
lexOneLineComment input =
    case input of
        [] -> mainLexer []
        ('\n':cs) -> do
            nextPos '\n'
            mainLexer cs
        (c:cs) -> do
            nextPos c
            lexOneLineComment cs

lexMultiLineComment :: [SourcePos] -> Int -> Lexer
lexMultiLineComment starts level input =
    case input of 
        '-':'}':cs 
            | level == 0 -> do
                incPos 2
                mainLexer cs
            | otherwise ->  do
                incPos 2
                lexMultiLineComment (tail starts) (level - 1) cs
        '{':'-':cs -> do
            pos <- getPos
            lexerWarning (NestedComment (head starts)) pos
            incPos 2
            lexMultiLineComment (pos:starts) (level+1) cs
        c:cs -> do
            nextPos c
            lexMultiLineComment starts level cs
        [] -> 
            lexerError UnterminatedComment (head starts)
            -- at end-of-file show the most recently opened comment

-----------------------------------------------------------
-- Utility functions
-----------------------------------------------------------

isSymbol :: Char -> Bool
isSymbol c = elem c symbols
                                  
isLetter :: Char -> Bool
isLetter '\'' = True
isLetter '_'  = True
isLetter c    = myIsAlphaNum c

-- write our own isLower.. so that we don't accept funny symbols
myIsLower :: Char -> Bool
myIsLower c = c >= 'a' && c <= 'z'

myIsUpper :: Char -> Bool
myIsUpper c = c >= 'A' && c <= 'Z'

myIsDigit :: Char -> Bool
myIsDigit c = c >= '0' && c <= '9'

myIsAlphaNum :: Char -> Bool
myIsAlphaNum c = myIsLower c || myIsUpper c || myIsDigit c

myIsSpace :: Char -> Bool
myIsSpace c = c == ' ' || c == '\n' || c == '\t' || c == '\r'

isName :: String -> Bool
isName [] = False
isName (hd:tl) = (myIsUpper hd || myIsLower hd) && all isLetter tl

-----------------------------------------------------------
-- Constants
-----------------------------------------------------------

escapeChars :: String
escapeChars = "\\nabfnrtv\"'"

symbols :: String
symbols = "!#$%&*+./<=>?@^|-~:\\"

keywords :: [String]
keywords = 
    [ "let", "in", "do", "where", "case", "of", "if"
    , "then", "else", "data", "type", "module", "import", "hiding"
    , "infix", "infixl", "infixr", "_", "deriving"
    , "class", "instance", "default", "newtype" -- not supported
    ]

reservedConSyms :: [String]
reservedConSyms =
    [ "::" ]

reservedVarSyms :: [String]
reservedVarSyms =
    [ "=>", "->", "<-", "..", "-", "-.", "@", "=", "\\", "|", "~" ]

specialsWithoutBrackets :: String
specialsWithoutBrackets = 
    ",`;" 

reserveStrategyNames :: [Token] -> [Token]
reserveStrategyNames tokens = 
  map (\token@(pos, lexeme) -> case lexeme of 
                   LexVar s    | s `elem` strategiesKeywords -> (pos, LexKeyword s)
                   LexVarSym s | s == "=="                   -> (pos, LexResVarSym s)
                   LexConSym s | s == ":"                    -> (pos, LexResConSym s)
                   _ -> token
      ) 
      tokens

strategiesKeywords :: [String]
strategiesKeywords = [ "phase", "constraints", "siblings" ]
