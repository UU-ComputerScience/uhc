{-| Module      :  LexerMonad
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.LexerMonad
    ( LexerMonad
    , getPos, incPos, nextPos, addPos
    , openBracket, closeBracket, checkBracketsAtEOF
    , lexerError, lexerWarning
    , runLexerMonad
    ) where

import Helium.Parser.LexerMessage
import Text.ParserCombinators.Parsec.Pos

type Bracket = (SourcePos, Char)

-- Output monad: [LexerWarning]
-- State monad: SourcePos and [Bracket]
newtype LexerMonad a = 
    LM (SourcePos -> [Bracket] -> 
        Either LexerError (a, [LexerWarning], SourcePos, [Bracket]))

unLM (LM x) = x

bindLM :: LexerMonad a -> (a -> LexerMonad b) -> LexerMonad b
bindLM (LM f) g = 
    LM (\pos brackets -> 
        case f pos brackets of
            Left err -> Left err
            Right (a, warnings, pos2, brackets2) -> 
                case unLM (g a) pos2 brackets2 of
                    Left err -> Left err
                    Right (b, moreWarnings, pos3, brackets3) ->
                        Right (b, warnings ++ moreWarnings, pos3, brackets3))

returnLM :: a -> LexerMonad a
returnLM x = LM (\pos brackets -> Right (x, [], pos, brackets))

instance Monad LexerMonad where
    (>>=) = bindLM
    return = returnLM

runLexerMonad :: String -> LexerMonad a -> Either LexerError (a, [LexerWarning])
runLexerMonad fileName (LM f) = 
    case f (initialPos fileName) [] of
        Left err -> Left err
        Right (a, warnings, _, _) -> Right (a, keepOneTabWarning warnings)

getPos :: LexerMonad SourcePos
getPos = LM (\pos brackets -> Right (pos, [], pos, brackets))

incPos :: Int -> LexerMonad ()
incPos i = LM (\pos brackets -> Right ((), [], addPos i pos, brackets))

nextPos :: Char -> LexerMonad ()
nextPos c = LM (\pos brackets -> Right ( (), [], updatePosChar pos c, brackets ))

lexerError :: LexerErrorInfo -> SourcePos -> LexerMonad a
lexerError err pos = 
    LM (\_ _ -> Left (LexerError pos err))

lexerWarning :: LexerWarningInfo -> SourcePos -> LexerMonad ()
lexerWarning warning warningPos = 
    LM (\pos brackets -> 
        Right ((), [LexerWarning warningPos warning], pos, brackets))
    
addPos :: Int -> SourcePos -> SourcePos
addPos i pos = incSourceColumn pos i

openBracket :: Char -> LexerMonad ()
openBracket c = LM (\pos brackets ->
    Right ( (), [], pos, (pos, c) : brackets ))

closeBracket :: Char -> LexerMonad ()
closeBracket c = LM (\pos brackets ->
    case brackets of
        [] -> Left (LexerError pos (TooManyClose c))
        (pos2, c2):rest
            | matchBracket c2 c ->
                Right ((), [], pos, rest)
            | otherwise ->
                Left (LexerError pos (UnexpectedClose c pos2 c2))        
    )
    
checkBracketsAtEOF :: LexerMonad ()
checkBracketsAtEOF = LM (\pos brackets ->
    case brackets of
        [] -> Right ((), [], pos, [])
        _  -> Left (LexerError pos (StillOpenAtEOF brackets))
    )
    
matchBracket :: Char -> Char -> Bool
matchBracket '(' ')' = True
matchBracket '[' ']' = True
matchBracket '{' '}' = True
matchBracket _ _ = False
