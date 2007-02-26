{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
--
-- A representation of type schemes. A type scheme is a (qualified) type
-- with a number of quantifiers (foralls) in front of it. A partial mapping 
-- from type variable (Int) to their name (String) is preserved.
--
-----------------------------------------------------------------------------

module Top.Types.Schemes where

import Top.Types.Primitive
import Top.Types.Quantification
import Top.Types.Qualification
import Top.Types.Substitution
import Top.Types.Synonym
import Top.Types.Unification
import Top.Types.Classes
import Data.List
import qualified Data.Map as M

----------------------------------------------------------------------
-- * Type schemes

-- |A type scheme consists of a list of quantified type variables, a finite map 
-- that partially maps these type variables to their original identifier, and a
-- qualified type.
type TpScheme = Forall QType
type QType    = Qualification Predicates Tp

-- |A type class to convert something into a type scheme
class IsTpScheme a where
   toTpScheme :: a -> TpScheme
   
instance IsTpScheme TpScheme where
   toTpScheme = id

instance IsTpScheme QType where
   toTpScheme = noQuantifiers
   
instance IsTpScheme Tp where
   toTpScheme = noQuantifiers . ([] .=>.)

----------------------------------------------------------------------
-- * Basic functionality for types and type schemes

-- |Determine the arity of a type scheme.    
arityOfTpScheme :: TpScheme -> Int
arityOfTpScheme = arityOfTp . unqualify . unquantify

genericInstanceOf :: OrderedTypeSynonyms -> ClassEnvironment -> TpScheme ->  TpScheme -> Bool
genericInstanceOf synonyms classes scheme1 scheme2 = 
   let -- monomorphic type variables are treated as constants
       s1 = skolemizeFTV scheme1
       s2 = skolemizeFTV scheme2
       -- substitution to fix the type variables in the first type scheme
       sub        = listToSubstitution (zip (quantifiers s1) [ TCon ('+':show i) | i <- [0 :: Int ..]])
       (ps1, tp1) = split (sub |-> unquantify s1)
       (ps2, tp2) = split (snd (instantiate 123456789 s2))
   in case mguWithTypeSynonyms synonyms tp1 tp2 of
         Left _         -> False
         Right (_,sub2) -> entailList synonyms classes ps1 (sub2 |-> ps2)

-- |Is the type scheme overloaded (does it contain predicates)?
isOverloaded :: TpScheme -> Bool
isOverloaded = not . null . qualifiers . unquantify

makeScheme :: [Int] -> Predicates -> Tp -> TpScheme
makeScheme monos preds tp = 
   let is  = ftv tp \\ monos
       p   = any (`elem` is) . ftv
   in quantify is (filter p preds .=>. tp)   

instantiateWithNameMap :: Int -> TpScheme -> (Int, Predicates, Tp) -- get rid of this function.
instantiateWithNameMap unique (Quantification (qs,nm,qtp)) = 
   let sub = listToSubstitution [ (i,TCon s) | (i,s) <- nm, i `elem` qs ]
       (u, qtp') = instantiate unique (Quantification (qs \\ (map fst nm), [], sub |-> qtp))
       (ps, tp) = split qtp'
   in (u, ps, tp)

instance (ShowQualifiers q, Show a) => ShowQuantors (Qualification q a)

-- |A sigma is a type scheme or a type scheme variable
type Scheme qs = Forall (Qualification qs Tp)

data Sigma qs  = SigmaVar    SigmaVar 
               | SigmaScheme (Scheme qs)
type SigmaVar  = Int

instance (ShowQualifiers qs, Substitutable qs) => Show (Sigma qs) where
   show (SigmaVar i)    = "s" ++ show i
   show (SigmaScheme s) = show s

instance Substitutable qs => Substitutable (Sigma qs) where   
   _   |-> sv@(SigmaVar _) = sv 
   sub |-> (SigmaScheme s) = SigmaScheme (sub |-> s)   
   
   ftv (SigmaVar _)    = []
   ftv (SigmaScheme s) = ftv s 

instance (Substitutable qs, ShowQualifiers qs) => ShowQuantors (Sigma qs) where
   showQuantorsWithout options sigma =
      case sigma of
         SigmaVar _     -> show sigma
         SigmaScheme ts -> showQuantorsWithout options ts
   
-- |A substitution for type scheme variables
type TpSchemeMap = M.Map SigmaVar TpScheme

type SigmaPreds = Sigma Predicates

class IsSigmaPreds a where
   toSigmaPreds :: a -> SigmaPreds
   
instance IsSigmaPreds SigmaPreds where toSigmaPreds = id 
instance IsSigmaPreds TpScheme   where toSigmaPreds = SigmaScheme . toTpScheme
instance IsSigmaPreds QType      where toSigmaPreds = SigmaScheme . toTpScheme
instance IsSigmaPreds Tp         where toSigmaPreds = SigmaScheme . toTpScheme
instance IsSigmaPreds Int        where toSigmaPreds = SigmaVar