-- error: attempt to infer infinite type because of x :: t, t === t -> ...

let  f = \x -> x x
 in  3
