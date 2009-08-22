{-| Module      :  CollectFunctionBindings
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.CollectFunctionBindings where

import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils
import Helium.Syntax.UHA_Range
import Helium.Utils.Utils

-- Assumption: each FunctionBindings contains exactly one FunctionBinding

decls :: Declarations -> Declarations
decls [] = []
decls (d@(Declaration_FunctionBindings _ [_]):ds) =
    let mn = nameOfDeclaration d
        (same, others) = span ((== mn) . nameOfDeclaration) (d:ds)
        fs = map functionBindingOfDeclaration same
    in Declaration_FunctionBindings 
        (mergeRanges (rangeOfFunctionBinding (head fs)) (rangeOfFunctionBinding (last fs)))
        fs
       :
       decls others
decls (Declaration_FunctionBindings _ _:_) =
    internalError "CollectFunctionBindings" "decls" "not exactly one function binding in FunctionBindings"
decls (d:ds) = d : decls ds

functionBindingOfDeclaration :: Declaration -> FunctionBinding
functionBindingOfDeclaration (Declaration_FunctionBindings _ [f]) = f
functionBindingOfDeclaration _ = 
    internalError "CollectFunctionBindings" "getFunctionBinding" "unexpected declaration kind"

rangeOfFunctionBinding :: FunctionBinding -> Range
rangeOfFunctionBinding (FunctionBinding_FunctionBinding r _ _) = r

nameOfDeclaration :: Declaration -> Maybe Name
nameOfDeclaration d =
    case d of 
        Declaration_FunctionBindings _ [FunctionBinding_FunctionBinding _ l _] ->
            Just (nameOfLeftHandSide l)
        _ -> Nothing

nameOfLeftHandSide :: LeftHandSide -> Name
nameOfLeftHandSide lhs =
    case lhs of
        LeftHandSide_Function _ n _ -> n
        LeftHandSide_Infix _ _ n _ -> n
        LeftHandSide_Parenthesized _ innerLhs _ -> nameOfLeftHandSide innerLhs
        