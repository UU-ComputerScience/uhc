\section{LL(1) financial contracts code}
\label{a1llcode}

>newtype O  a = O a -- or
>newtype A  a = A a -- and
>newtype P  a = P a -- (
>newtype Q  a = Q a -- )
>newtype G  a = G a -- give
>newtype N  a = N a -- one
>newtype Z  a = Z a -- zero
>newtype E  a = E a -- euro
>newtype D  a = D a -- dollar
>
>newtype S   a = S   (Contract               -> a)
>newtype S'  a = S'  ((Contract -> Contract) -> a)
>newtype AC  a = AC  (Contract               -> a)
>newtype AC' a = AC' ((Contract -> Contract) -> a)
>newtype C   a = C   (Contract               -> a)
>newtype CU  a = CU  (Currency               -> a)
>
>quote = lift (S  (\x -> x))
>
>class Or     old new | old -> new where or    :: old -> CPS new
>class And    old new | old -> new where and   :: old -> CPS new
>class Open   old new | old -> new where p     :: old -> CPS new
>class Close  old new | old -> new where q     :: old -> CPS new
>class End    old                  where end   :: old -> Contract
>class Give   old new | old -> new where give  :: old -> CPS new
>class One    old new | old -> new where one   :: old -> CPS new
>class Zero   old new | old -> new where zero  :: old -> CPS new
>class Euro   old new | old -> new where euro  :: old -> CPS new
>class Dollar old new | old -> new where dollar:: old -> CPS new
>
>
>instance Or  (S'  a) (AC (S'  a)) where
>  or   (S'  ctx ) = or   (O (AC (\t -> S'  (\e' -> ctx (\l -> e' (Or  l t))))))
>instance (Or  a a') => Or  (AC' a) a' where
>  or   (AC' ctx ) = or   (ctx (\e -> e))
>instance Or  (O a) a where
>  or   (O ctx ) = lift ctx
>
>instance And (AC' a) (C (AC' a)) where
>  and   (AC' ctx ) = and   (A (C (\f -> AC' (\t' -> ctx (\l -> t' (And l f ))))))
>instance And (A a) a where
>  and   (A ctx ) = lift ctx
>
>instance Open (S  a) (S  (Q (AC' (S'  a)))) where
>  p (S  ctx ) = p (AC (\t -> S'  (\e' -> ctx (e' t))))
>instance Open (AC a) (S  (Q (AC' a))) where
>  p (AC ctx ) = p (C (\f -> AC' (\t' -> ctx (t' f ))))
>instance Open (C a) (S  (Q a)) where
>  p (C ctx ) = p (P (S  (\e -> Q (ctx e))))
>instance Open (P a) a where
>  p (P ctx ) = lift ctx
>
>instance (Close a a') => Close (S'  a) a' where
>  q (S'  ctx ) = q (ctx (\e -> e))
>instance (Close a a') => Close (AC' a) a' where
>  q (AC' ctx ) = q (ctx (\e -> e))
>instance Close (Q a) a where
>  q (Q ctx ) = lift ctx
>
>instance (End a) => End (S'  a) where
>  end (S'  ctx ) = end (ctx (\e -> e))
>instance (End a) => End (AC' a) where
>  end (AC' ctx ) = end (ctx (\e -> e))
>instance End Contract where
>  end e = e
>
>instance Give (S  a) (C (AC' (S'  a))) where
>  give (S  ctx) = give (AC (\t -> S'  (\e' -> ctx (e' t))))
>instance Give (AC a) (C (AC' a)) where
>  give (AC ctx) = give (C (\f -> AC' (\t' -> ctx (t' f ))))
>instance Give (C a) (C a) where
>  give (C ctx) = give (G (C (\c -> ctx (Give c))))
>instance Give (G a) a where
>  give (G ctx) = lift ctx
>
>instance One (S  a) (CU (AC' (S'  a))) where
>  one (S  ctx) = one (AC (\t -> S'  (\e' -> ctx (e' t))))
>instance One (AC a) (CU (AC' a)) where
>  one (AC ctx) = one (C (\f -> AC' (\t' -> ctx (t' f ))))
>instance One (C a) (CU a) where
>  one (C ctx) = one (N (CU (\c -> ctx (One c))))
>instance One (N a) a where
>  one (N ctx) = lift ctx
>
>instance Zero (S  a) (AC' (S'  a)) where
>  zero (S  ctx) = zero (AC (\t -> S'  (\e' -> ctx (e' t))))
>instance Zero (AC a) (AC' a) where
>  zero (AC ctx) = zero (C (\f -> AC' (\t' -> ctx (t' f ))))
>instance Zero (C a) a where
>  zero (C ctx) = zero (Z (ctx Zero))
>instance Zero (Z a) a where
>  zero (Z ctx) = lift ctx
>
>instance Euro (CU a) a where
>  euro (CU ctx) = euro (E (ctx Euro))
>instance Euro (E a) a where
>  euro (E ctx)  = lift ctx
>
>instance Dollar (CU a) a where
>  dollar (CU ctx) = dollar (D (ctx Dollar))
>instance Dollar (D a) a where
>  dollar (D ctx)  = lift ctx
