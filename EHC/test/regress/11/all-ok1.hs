-- all ok

data [] a = ''[]'' | a : [a]

type String = [Char]

instance C Char where
  c x = x

class C a where
  c :: a -> a

type CS a = C a => a -> [a]

f1 :: CS Char
f1 ch = [c ch]

v1 = f1 'a'

main = v1

