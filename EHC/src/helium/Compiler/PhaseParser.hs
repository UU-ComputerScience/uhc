{-| Module      :  PhaseParser
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.PhaseParser(phaseParser) where

import Helium.Compiler.CompileUtils
import Helium.Parser.LexerToken(Token)
import qualified Helium.Parser.Parser as Parser(module_)
import Helium.Parser.ParseLibrary(runHParser)
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