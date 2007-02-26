{-| Module      :  PhaseParser
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module PhaseParser(phaseParser) where

import CompileUtils
import LexerToken(Token)
import qualified Parser(module_)
import ParseLibrary(runHParser)
import Text.ParserCombinators.Parsec.Error (ParseError)

phaseParser :: 
   String -> [Token] -> [Option] -> 
   Phase ParseError Module
phaseParser fullName tokens options = do
    enterNewPhase "Parsing" options
    case runHParser Parser.module_ fullName tokens True of
        Left parseError -> do
            return (Left [parseError])
        Right m ->
            return (Right m)