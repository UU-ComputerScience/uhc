{-# OPTIONS -fglasgow-exts #-}
module Greibach where

import CPS

{----------------------------------------------------------

4.1 Greibach normal form

A context-free grammar is in the Greibach normal form if all
productions are of the form A -> aW and is unambiguous if
A1 -> aW1 and A2 -> aW2 satisfies A1 = A2 => W1 = W2.

-----------------------------------------------------------}

type Var = String

data Stat = Let   Var Var
          | If    Var Stat Stat
          | While Var Stat
          | Begin [Stat]
          deriving Show
          
newtype S a = S (Stat   -> a)
newtype A a = A (          a)
newtype E a = E (Var    -> a)
newtype R a = R ([Stat] -> a)

class Id old new | old -> new where
   var :: old -> String -> CPS new
--   var :: String -> Arrow old new
   

instance Id (S a) (A (E a))  where
   var (S ctx) l = lift (A (E (\r                -> ctx (Let l r    ))))
   
iff      (S ctx) = lift (E (\c -> S (\t -> S (\e -> ctx (If c t e  )))))   

while    (S ctx) = lift (E (\c -> S (\b          -> ctx (While c b  ))))

begin    (S ctx) = lift (S (\s -> R (\r          -> ctx (Begin (s:r)))))

assign   (A ctx) = lift ctx

instance Id (E a) a  where
   var (E ctx) l = lift (ctx l)
   
end      (R ctx) = lift (ctx [])

col      (R ctx) = lift (S (\s -> R (\r          -> ctx (s:r))))

quote = lift (S (\s -> s))

endquote s = s

