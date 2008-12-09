-- duplicate intro in subsequent let's, allowed in EH

let id = \x -> x
in
let  v = id 3
in
let  v = id 4
in   v
