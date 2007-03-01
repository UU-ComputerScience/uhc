{-| Module      :  Matchers
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable

	Matching expressions

    (directives based on "Scripting the Type Inference Process", ICFP 2003)
-}

module Helium.StaticAnalysis.Directives.Matchers where

import Helium.Syntax.UHA
import Helium.StaticAnalysis.Messages.Messages () -- instance Eq Name

-------------------------------------------------------------
-- Expression

match_Expression_Literal :: Literal -> Expression -> Maybe ()
match_Expression_Literal l1 expr = 
   case expr of
      Expression_Literal _ l2 | l1 `eqLiteral` l2 -> Just ()
      _                                           -> Nothing

match_Expression_Variable :: Name -> Expression -> Maybe ()
match_Expression_Variable n1 expr =
   case expr of
      Expression_Variable _ n2 | n1 == n2 -> Just ()
      _                                   -> Nothing   

match_Expression_Constructor :: Name -> Expression -> Maybe ()
match_Expression_Constructor n1 expr = 
   case expr of
      Expression_Constructor _ n2 | n1 == n2 -> Just ()
      _                                      -> Nothing 

match_Expression_NormalApplication :: Expression -> Maybe (Expression, Expressions)
match_Expression_NormalApplication expr = 
   case expr of
      Expression_NormalApplication _ e es -> Just (e,es)
      _                                   -> Nothing
      
match_Expression_InfixApplication :: Expression -> Maybe (MaybeExpression, Expression, MaybeExpression)
match_Expression_InfixApplication expr = 
   case expr of
      Expression_InfixApplication _ me1 e me2 -> Just (me1,e,me2)
      _                                       -> Nothing
      
match_Expression_If :: Expression -> Maybe (Expression,Expression,Expression)
match_Expression_If expr = 
   case expr of
      Expression_If _ e1 e2 e3 -> Just (e1,e2,e3)
      _                        -> Nothing

match_Expression_Lambda :: Expression -> Maybe (Patterns,Expression)
match_Expression_Lambda expr = 
   case expr of
      Expression_Lambda _ p e -> Just (p,e)
      _                       -> Nothing

match_Expression_Case :: Expression -> Maybe (Expression,Alternatives)
match_Expression_Case expr = 
   case expr of
      Expression_Case _ e as -> Just (e,as)
      _                      -> Nothing

match_Expression_Let :: Expression -> Maybe (Declarations,Expression)
match_Expression_Let expr = 
   case expr of
      Expression_Let _ ds e -> Just (ds,e)
      _                     -> Nothing 

match_Expression_Do :: Expression -> Maybe (Statements)
match_Expression_Do expr = 
   case expr of
      Expression_Do _ ss -> Just (ss)
      _                  -> Nothing 

match_Expression_List :: Expression -> Maybe (Expressions)
match_Expression_List expr = 
   case expr of
      Expression_List _ es -> Just (es)
      _                    -> Nothing 

match_Expression_Tuple :: Expression -> Maybe (Expressions)
match_Expression_Tuple expr = 
   case expr of
      Expression_Tuple _ es -> Just (es)
      _                     -> Nothing 

match_Expression_Comprehension :: Expression -> Maybe (Expression,Qualifiers)
match_Expression_Comprehension expr = 
   case expr of
      Expression_Comprehension _ e qs -> Just (e,qs)
      _                               -> Nothing 

match_Expression_Typed :: Expression -> Maybe (Expression,Type)
match_Expression_Typed expr = 
   case expr of
      Expression_Typed _ e t -> Just (e,t)
      _                      -> Nothing 

match_Expression_Enum :: Expression -> Maybe (Expression,MaybeExpression,MaybeExpression)
match_Expression_Enum expr = 
   case expr of
      Expression_Enum _ e me1 me2 -> Just (e,me1,me2)
      _                           -> Nothing 

match_Expression_Negate :: Expression -> Maybe (Expression)
match_Expression_Negate expr = 
   case expr of
      Expression_Negate _ e -> Just (e)
      _                     -> Nothing 

match_Expression_NegateFloat :: Expression -> Maybe (Expression)
match_Expression_NegateFloat expr = 
   case expr of
      Expression_NegateFloat _ e -> Just (e)
      _                          -> Nothing 

-------------------------------------------------------------
-- Expressions

match_Expressions_Cons :: Expressions -> Maybe (Expression,Expressions)
match_Expressions_Cons exprs = 
   case exprs of
      e:es -> Just (e,es)
      _    -> Nothing
      
match_Expressions_Nil :: Expressions -> Maybe ()
match_Expressions_Nil exprs =
   case exprs of
      [] -> Just ()
      _  -> Nothing

-------------------------------------------------------------
-- MaybeExpression

match_MaybeExpression_Just :: MaybeExpression -> Maybe (Expression)
match_MaybeExpression_Just mexpr =
   case mexpr of
      MaybeExpression_Just e -> Just e
      _                      -> Nothing

match_MaybeExpression_Nothing :: MaybeExpression -> Maybe ()
match_MaybeExpression_Nothing mexpr = 
   case mexpr of
      MaybeExpression_Nothing -> Just ()
      _                       -> Nothing

-------------------------------------------------------------

eqLiteral :: Literal -> Literal -> Bool
eqLiteral (Literal_Char   _ x) (Literal_Char   _ y) = x == y
eqLiteral (Literal_Float  _ x) (Literal_Float  _ y) = x == y
eqLiteral (Literal_Int    _ x) (Literal_Int    _ y) = x == y
eqLiteral (Literal_String _ x) (Literal_String _ y) = x == y
eqLiteral _                    _                    = False      
