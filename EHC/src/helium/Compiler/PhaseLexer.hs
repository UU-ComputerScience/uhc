{-| Module      :  PhaseLexer
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.PhaseLexer(phaseLexer) where

import Helium.Compiler.CompileUtils
import Helium.Parser.LexerToken(Token)
import Helium.Parser.Lexer
import Helium.Parser.LayoutRule(layout)

phaseLexer :: 
   String -> String -> [Option] -> 
   Phase LexerError ([LexerWarning], [Token])

phaseLexer fullName contents options = do
    enterNewPhase "Lexing" options

    case lexer fullName contents of 
        Left lexError ->
           return (Left [lexError])
        Right (tokens, lexerWarnings) -> do
            let tokensWithLayout = layout tokens
            when (DumpTokens `elem` options) $ do
                putStrLn (show tokensWithLayout)
            let warnings = filterLooksLikeFloatWarnings lexerWarnings tokensWithLayout
            return (Right (warnings, tokensWithLayout))

-- Throw away the looks like float warnings between the keywords "module"
-- and "where".
filterLooksLikeFloatWarnings :: [LexerWarning] -> [Token] -> [LexerWarning]
filterLooksLikeFloatWarnings warnings tokens = 
   case tokens of
      (_, LexKeyword "module"):_ ->
         case dropWhile test tokens of
            (sp, _):_ -> filter (pred sp) warnings
            _         -> warnings
      _               -> warnings
 where
   test (_, t) = t /= LexKeyword "where"
   pred sp1 (LexerWarning sp2 w) = 
      not (sp2 <= sp1 && isLooksLikeFloatWarningInfo w)