-- |
-- Analysis functions over the Cil AST.
--

module Language.Cil.Analysis (
    instructions
  ) where

import Language.Cil.Syntax

class Ast a where
  -- A concatenated list of all instructions.
  instructions :: a -> [Instr]

instance Ast Assembly where
  instructions (Assembly _ _ td) = concatMap instructions td

instance Ast TypeDef where
  instructions (Class _ _ _ _ cd) = concatMap instructions cd

instance Ast ClassDecl where
  instructions (FieldDef  _)  = []
  instructions (MethodDef md) = instructions md

instance Ast MethodDef where
  instructions (Constructor _ _ md) = [ i | Instr i <- md ]
  instructions (Method _ _ _ _ md)  = [ i | Instr i <- md ]

