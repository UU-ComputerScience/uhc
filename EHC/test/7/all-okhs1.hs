-- record, data fields
-- %% inline test (prefix1) --

data T a = T {x, y :: a}
x1 = T 3 4
x2 = T { x = 3, y = 4 }
r1 = (a=333,b=4)
r2 :: (a::Int,b::Int)
r3 :: Rec {| a::Int,b::Int |}
r2 = r1
s2 :: {< a::Int,b::Int >}
v1 = r1#a
f (c=a,b) = a

main = f (c = v1, b = 3)
