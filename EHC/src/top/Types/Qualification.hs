-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
--
-- Qualification of types (for instance, predicates to deal with type classes).
--
-----------------------------------------------------------------------------

module Top.Types.Qualification where

import Top.Types.Primitive
import Top.Types.Substitution
import Data.List

-----------------------------------------------------------------------------
-- * Qualification

newtype Qualification q a = Qualification (q, a)

split :: Qualification q a -> (q, a)
split (Qualification t) = t

infixr 2 .=>.

(.=>.) :: q -> a -> Qualification q a 
(.=>.) = curry Qualification

qualifiers :: Qualification q a -> q
qualifiers = fst . split

unqualify :: Qualification q a -> a
unqualify = snd . split

qualify :: (Substitutable context, Substitutable q, Substitutable a) => context -> [q] -> a -> Qualification [q] a
qualify context preds tp = 
   let is  = ftv tp \\ ftv context
       p   = any (`elem` is) . ftv
   in (filter p preds .=>. tp)

instance (Substitutable q, Substitutable a) => Substitutable (Qualification q a) where
   sub |-> (Qualification t) = Qualification (sub |-> t)
   ftv     (Qualification t) = ftv t

instance (HasTypes q, HasTypes a) => HasTypes (Qualification q a) where
   getTypes      (Qualification t) = getTypes t
   changeTypes f (Qualification t) = Qualification (changeTypes f t)

instance (ShowQualifiers q, Show a) => Show (Qualification q a) where
   show (Qualification (q, a)) = 
      showContext q ++ show a
      
class Show a => ShowQualifiers a where
   showQualifiers :: a -> [String]
   -- default definition
   showQualifiers = (:[]) . show

showContext :: ShowQualifiers a => a -> String
showContext = showContextSimple . showQualifiers

showContextSimple :: [String] -> String
showContextSimple []  = ""
showContextSimple [x] = x ++ " => "
showContextSimple xs  = "(" ++ concat (intersperse ", " xs) ++ ") => "
      
instance (ShowQualifiers a, ShowQualifiers b) => ShowQualifiers (a, b) where
   showQualifiers (a, b) = showQualifiers a ++ showQualifiers b

instance ShowQualifiers a => ShowQualifiers [a] where
   showQualifiers = concatMap showQualifiers