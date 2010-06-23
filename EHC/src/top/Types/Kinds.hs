-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
--
-- Kinds can be represented by a type.
--
-----------------------------------------------------------------------------

module Top.Types.Kinds where

import Top.Types.Primitive
import Top.Types.Substitution
import Top.Types.Quantification
import Top.Types.Schemes

type Kind       = Tp
type Kinds      = [Kind]
type KindScheme = TpScheme         

-- |Star is the kind of all values.
star :: Kind
star = TCon "*"

-- |In traditional kind inference systems, a kind cannot contain variables.
-- At some point in the inference process the kind variables are defaulted
-- to star.
defaultToStar :: Kind -> Kind
defaultToStar kind = 
   let sub = listToSubstitution [ (i, star) | i <- ftv kind ]
   in sub |-> kind

-- |A function to show kinds.
showKind :: Kind -> String
showKind kind = 
   let sub = listToSubstitution [ (i, TCon ('k':show i)) | i <- ftv kind ]
   in show (sub |-> kind)

showKindScheme :: KindScheme -> String
showKindScheme scheme = 
   let sub = listToSubstitution
                $  [ (i, TCon ('k':show j)) | (i, j) <- zip (quantifiers scheme) [1 :: Int ..] ] 
                ++ [ (i, TCon ("_k"++show i)) | i <- ftv scheme ]
   in show (sub |-> unquantify scheme)
