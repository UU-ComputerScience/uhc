%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.Exception}
%%]

%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})
%%]


%%[(8 codegen)

data  E a = Error String | Val a 
            deriving(Eq,Read,Show)

-- instance Monad

instance Monad E where

  (Val x)    >>= k  = k x
  (Error s)  >>= _  = Error s
  return            = Val


{- old definitions

unitE = return
bindE = >>=   -}


-- primitive  functions (not abstract)

errorE :: String -> E a
errorE s = Error s

escapeE :: E a -> E a -> E a
k `escapeE` m = case k of
                     Error _ -> m
                     Val _ -> k


elimE :: E a -> (a -> b) -> (String -> b) -> b
elimE x f g = case x of
                 Error s -> g s
                 Val a -> f a


-- general functions (abstract)

thenE :: E a -> E b -> E b
thenE = (>>)


updateErrorE :: E a -> String -> E a
updateErrorE e str = elimE e return (errorE . (++str))

handleE :: E a -> a -> a
handleE k x = caseE k id x


caseE :: E a -> (a -> b) -> b -> b
caseE x f v = elimE x f (\ _ -> v)

consE :: a -> E [a] -> E [a]
a `consE` e = e >>= (\as -> return (a:as))

zipE :: [a] -> [b] -> E [(a,b)]
zipE [] [] = return []
zipE (x:l) (y:k) = (x,y) `consE` zipE l k
zipE _ _ = errorE "zipE"

lookupE x [] = errorE (cannot_Find_Label x)
lookupE x ((y,u):l1) = if x == y then return u else lookupE x l1

lookupintE :: Int -> [(Int,a)] -> E a
lookupintE = lookupE

tryAllE :: (a-> E()) -> [a] -> E()
tryAllE = mapM_


tryFirstE :: (a -> E b) -> [a] -> E b
tryFirstE f [] = errorE "tryFirstE"
tryFirstE f (x:xs) = caseE (f x) return (tryFirstE f xs)

tryUntilE :: (a -> E a) -> [a] -> E [a]
tryUntilE f [] = errorE "tryUntilE"
tryUntilE f (x:xs) =
 caseE (f x) 
       (\ x1 -> return (x1:xs)) 
       (tryUntilE f xs >>= \ xs1 -> return (x:xs1))


-- These were defined 

-- add_updateE :: String -> a -> [(String,[a])] -> E [(String,[a])]
add_updateE s u [] = errorE "add_updateE"
add_updateE s u ((s1,l1):rest) =
 if s == s1 
      then return ((s,l1++[u]):rest)
      else add_updateE s u rest >>= \ rest1 ->
           return ((s1,l1):rest1)

-- dangerous function

coerceE :: E a -> a
coerceE u = elimE u (\x -> x) error


unitE x  = return x
bindE x = (>>=) x


%%]