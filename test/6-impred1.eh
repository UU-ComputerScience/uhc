let  data L a = N | C a (L a)
     id = \x -> x
in
let  v1 = C id N
     v2 = C (id::a->a) N
in   v1
