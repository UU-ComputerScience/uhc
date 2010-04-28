{-
20071129, Arthur:

dit is wat ik probeer
hugs keurt het af met een melding over ontsnappende existentieele types, en dat is maar goed ook
ik vroeg me af of ehc dit goedkeurt, omdat ehc ontsnappende dingen niet erg vindt
zou wel een probleem zijn denk ik


-}

data E s Ê= forall x . Lees x => ÊX ((s -> x) -> Int)
Ê Ê Ê Ê Ê | Y s
data A s = A (E s)


fun :: (forall s . Lees s => A s) -> Char
fun es =
Ê Ê let A e = es
Ê Ê in case e of
Ê Ê Ê Ê Ê X f -> k '1' (f i)
Ê Ê Ê Ê Ê Y s -> k '2' Ê(same (lees '1') s)


same :: a -> a -> a
same x y = x

i x = x
k x y = x

undef = undef

class Lees a where
Ê lees :: Char -> a

main = undef
