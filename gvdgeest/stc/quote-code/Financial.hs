{-
The financial combinators from:
Composing contracts: an adventure in financial engineering

The grammar in Reverse Polish Notation:
-}

module Financial where
import qualified Prelude
import CPS

data Contract = Zero 
              | One         Currency
              | Give        Contract
              | And         Contract Contract
              | Or          Contract Contract
              | Truncate    Date     Contract
              | Then        Contract Contract
              | Scale       Prelude.Double   Contract
              | Get         Contract
              | Anytime     Contract
               deriving Prelude.Show

data Currency = USD
              | GBD
              | EUR
              | YEN
               deriving Prelude.Show

data Date   = Date Prelude.Int Prelude.Int Prelude.Int
               deriving Prelude.Show

newtype C a = C (Contract -> a)
newtype D a = D (Date     -> a)

-- Of cource I could write the definitions below shorter, but this is more clear
zero     (C ctx)   = ret (ctx Zero)
one      (C ctx) c = ret (ctx (One c))
give     (C ctx)   = ret (C (\c -> ctx (Give c)))
and      (C ctx)   = ret (C (\c -> C (\d -> ctx (And c d))))
or       (C ctx)   = ret (C (\c -> C (\d -> ctx (Or  c d))))
truncate (C ctx)   = ret (D (\d -> C (\c -> ctx (Truncate d c))))
tthen    (C ctx)   = ret (C (\c -> C (\d -> ctx (Then c d))))
scale    (C ctx) d = ret (C (\c -> ctx (Scale d c)))
get      (C ctx)   = ret (C (\c -> ctx (Get  c)))
anytime  (C ctx)   = ret (C (\c -> ctx (Anytime c)))
anti     (C ctx) c = ret (ctx c)

date     (D ctx) y m d = ret (ctx (Date y m d))

quote = ret (C Prelude.id)
end c = c


