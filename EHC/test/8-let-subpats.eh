let data L a = N | C a (L a)
in
let ab@(a,C b c@(C _ (C _ _))) = (3,C 4 (C 5 N))
in  b
