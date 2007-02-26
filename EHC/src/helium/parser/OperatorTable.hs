{-| Module      :  OperatorTable
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module OperatorTable
    (   Assoc(..)
    ,   OperatorTable
    ,   assoc, prio
    ) where

import UHA_Syntax
import UHA_Utils ()
import qualified Data.Map as M

type OperatorTable = M.Map Name (Int, Assoc)

-- From ParsecExpr
data Assoc              = AssocNone
                        | AssocLeft
                        | AssocRight

assoc :: OperatorTable -> Name -> Assoc
assoc ops name = 
    case M.lookup name ops of
        Nothing -> AssocLeft -- default associativity, if unspecified
        Just (_, a) -> a

prio :: OperatorTable -> Name -> Int
prio ops name = 
    case M.lookup name ops of
        Nothing        -> 9 -- default priority, if unspecified
        Just    (p, _) -> p

instance Eq Assoc where
    AssocLeft  == AssocLeft  = True
    AssocRight == AssocRight = True
    AssocNone  == AssocNone  = True
    _          == _          = False

instance Show Assoc where
   show AssocNone  = "infix"
   show AssocLeft  = "infixl"
   show AssocRight = "infixr"