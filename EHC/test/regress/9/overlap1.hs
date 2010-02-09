-- choice of most specific instance

data [] a = ''[]'' | a : [a]

class C a where
  c :: a -> a

instance dCInt <: C Int where
  c x = x
instance dCList <: C a => C [a] where
  c x = x
instance dCListInt <: C [Int] where
  c x = x

v1 = c [2]
main = v1
