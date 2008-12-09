{-| Module      :  OperatorTable
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Parser.OperatorTable
    (   Assoc(..)
    ,   OperatorTable
    ,   assoc, prio
    ) where

import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils ()
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