{-| Module      :  TS_Parser
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    The parser of a .type file.
	
	(directives based on "Scripting the Type Inference Process", ICFP 2003) 
-}

module TS_Parser where
 
-- UHA
import UHA_Syntax
import UHA_Utils (nameFromString)
import qualified UHA_Pretty as PP
-- Typing strategies
import TS_Syntax
import TS_CoreSyntax
import qualified TS_ToCore
-- Parser/Lexer
import Lexer (Token, Lexeme)
import ParseLibrary hiding (satisfy)
import Parser (exp0, type_, atype)
import qualified ResolveOperators (expression)
import Text.ParserCombinators.Parsec
-- Rest
import Data.List (intersperse)
import Data.IORef
import Data.Char
import OperatorTable
import Utils (internalError)

parseTypingStrategies :: OperatorTable -> String -> [Token] -> Either ParseError TypingStrategies
parseTypingStrategies operatorTable filename tokens = 
   runHParser (many parseTypingStrategy) filename tokens True {- wait for EOF -}
  
  where

   parseTypingStrategy :: HParser TypingStrategy
   parseTypingStrategy = 
      do lexSIBLINGS
         names <- commas1 (var <|> varop <|> con <|> conop <|> special)
         lexSEMI         
         return (TypingStrategy_Siblings names)
      <|>   
      do typerule    <- parseTypeRule 
         constraints <- many parseConstraint
         lexSEMI  
         return (TypingStrategy_TypingStrategy typerule constraints)
      
   parseTypeRule :: HParser TypeRule
   parseTypeRule =         
      do judgements <- many1 parseJudgement
         lexSEMI 
         let (premises, conclusion) = (init judgements, last judgements)
         return (TypeRule_TypeRule (map judgementToSimpleJudgement premises) conclusion)

   parseJudgement :: HParser Judgement
   parseJudgement =         
      do expression <- exp0 
         lexCOLCOL
         exprType   <- type_
         lexSEMI      
         let resolvedExpression = ResolveOperators.expression operatorTable expression
         return (Judgement_Judgement resolvedExpression exprType)     

   parseConstraint :: HParser UserStatement
   parseConstraint =
      do -- enter a new phase
         lexPHASE
         phase <- fmap read lexInt
         return (UserStatement_Phase (fromInteger phase))
      <|>
      do -- constraint set of meta-variable
         lexCONSTRAINTS         
         name <- varid
         return (UserStatement_MetaVariableConstraints name)
      <|> 
      parseUserConstraint
      
   parseUserConstraint :: HParser UserStatement
   parseUserConstraint = try pPredicate <|> pEquality
   
    where
     pPredicate =
        do -- user predicate
           predClass <- con
           predType <- atype
           lexCOL
           msgLines  <- many1 lexString
           let message = concat (intersperse "\n" msgLines)
           return (UserStatement_Pred predClass predType message)
           
     pEquality =
        do -- user equality constraint
           leftType  <- type_
           lexASGASG
           rightType <- type_
           lexCOL
           msgLines  <- many1 lexString
           let message = concat (intersperse "\n" msgLines)
           return (UserStatement_Equal leftType rightType message)

special ::  GenParser (SourcePos,Lexeme) SourcePos Name
special =  do lexCOL    ; return (nameFromString ":")
       <|> do lexASGASG ; return (nameFromString "==")

judgementToSimpleJudgement :: Judgement -> SimpleJudgement
judgementToSimpleJudgement judgement = 
   case judgement of
      Judgement_Judgement (Expression_Variable _ name) tp 
         -> SimpleJudgement_SimpleJudgement name tp
      Judgement_Judgement expression                   tp 
         -> internalError "TS_Parser.hs" "judgementToSimpleJudgement" 
               ("the following expression should have been a meta-variable: "++showExpression expression)
      
showExpression :: Expression -> String
showExpression = show . PP.sem_Expression
