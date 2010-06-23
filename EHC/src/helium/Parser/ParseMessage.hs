{-| Module      :  ParseMessage
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.ParseMessage() where

import Helium.StaticAnalysis.Messages.Messages hiding (Message)
import Helium.Syntax.UHA(Range(..), Position(..))
import Helium.Utils.Texts as Texts

import Text.ParserCombinators.Parsec.Error
import Text.ParserCombinators.Parsec.Pos

instance HasMessage ParseError where
    getMessage pe = 
        let msgs = errorMessages pe in
        MessageOneLiner (MessageString (Texts.parserSyntaxError ++ ": ")) :
        map (MessageOneLiner . MessageString . ("    "++)) (
            ( filter (not . null)
            . lines
            . showErrorMessages 
                    Texts.parserOr 
                    Texts.parserUnknown
                    Texts.parserExpecting 
                    Texts.parserUnexpected 
                    Texts.parserEndOfInput
            ) msgs
        )
    getRanges parseError =
        let pos = errorPos parseError
            name = sourceName pos; line = sourceLine pos; col = sourceColumn pos
            position = Position_Position name line col
        in [ Range_Range position position ]

