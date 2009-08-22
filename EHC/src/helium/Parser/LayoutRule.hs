{-| Module      :  LayoutRule
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.LayoutRule(layout) where

import Helium.Parser.LexerToken(Token, Lexeme(..), lexemeLength)
import Text.ParserCombinators.Parsec.Pos

layout :: [Token] -> [Token]
layout [] = []
layout input@((pos, lexeme):_) = optimise $
    case lexeme of 
        LexKeyword "module" -> 
            lay dummyToken [] input
        LexSpecial '{' ->
            lay dummyToken [] input
        _ ->
            (pos, LexInsertedOpenBrace) : 
            lay dummyToken [CtxLay (sourceColumn pos) False] input
    where
        zeroPos = setSourceColumn (setSourceLine pos 0) 0
        dummyToken = (zeroPos, LexVar "")

optimise :: [Token] -> [Token]
optimise (token1@(_, LexInsertedOpenBrace) : (_, LexInsertedSemicolon) : ts) =
    optimise (token1 : ts)
optimise (t:ts) = 
    t : optimise ts
optimise [] = []

data Context 
    = CtxLay Int Bool {- let? -}
    | CtxBrace
    deriving (Eq,Show)
    
--     previous token
--              enclosing contexts
lay :: Token -> [Context] -> [Token] -> [Token]

-- If we're in a CtxBrace and we see a '}', we leave that context.
-- If we see another token, we check to see if we need to add a
-- new context.
lay     _ 
        cc@(CtxBrace:cs) 
        input@(t@(pos, lexeme):ts)
    | lexeme == LexSpecial '}' =
        t : lay t cs ts
    | otherwise = 
        t : addContext t cc input 

-- If we're in a let layout context, an 'in' can end
-- the context, too.
lay     prevToken 
        (CtxLay _ True:cs) 
        (t@(pos, LexKeyword "in"):ts) 
    = (behind prevToken, LexInsertedCloseBrace) : t : lay t cs ts

-- If we're in a layout context and the new token is not on the 
-- same line as the previous, we check the column against the
-- context. If the new token is on the same line, we only need
-- to check whether a context has to be added.
lay     prevToken@(prevPos, _)
        cc@(CtxLay ctxCol _:cs) 
        input@(t@(pos, _):_) 
    | sourceLine pos > sourceLine prevPos = -- token on next line?
        if sourceColumn pos > ctxCol then -- indent more => nothing
            t : addContext t cc input
        else if sourceColumn pos == ctxCol then -- indent same => insert ';' 
            (behind prevToken, LexInsertedSemicolon) : t : addContext t cc input
        else -- indent less => insert brace, remove context and try again
            (behind prevToken, LexInsertedCloseBrace) : lay prevToken cs input
    | otherwise = -- token on same line
        t : addContext t cc input
        
lay _ _ [] = []

lay _ [] input@(t@(pos, _):_) = 
    t : addContext t [] input

behind :: Token -> SourcePos
behind (pos, lexeme) = incSourceColumn pos (lexemeLength lexeme)

addContext :: Token -> [Context] -> [Token] -> [Token]

-- If we see a '{' we add a CtxBrace context
addContext prevToken cs ((_, LexSpecial '{'):ts) = 
    lay prevToken (CtxBrace : cs) ts

-- If we see a 'do', 'where', 'of' or 'let' we add a context
-- and a '{' only if the next token is not a '{'
addContext prevToken cs 
        ((_, LexKeyword keyword):t2@(pos2, lexeme2):ts) 
    | keyword `elem` ["do", "where", "of","let"] =
        if lexeme2 == LexSpecial '{' then
            lay prevToken cs (t2:ts)
        else
            (pos2, LexInsertedOpenBrace) :
            lay prevToken 
                (CtxLay (sourceColumn pos2) (keyword == "let") : cs) 
                (t2:ts)
    | otherwise = 
        lay prevToken cs (t2:ts)

addContext prevToken cs (_:ts) =
    lay prevToken cs ts

addContext _ _ [] = []
