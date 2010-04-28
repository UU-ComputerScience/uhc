-- duplicate intro in same let's, not allowed in EH (and HS)

let id = \x -> x
in
let  v = id 3
     v = id 4
in   v
