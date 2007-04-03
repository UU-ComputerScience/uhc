main = member 't' "Gerrit"

member x [] = False
member x (y:ys) = x == y || member x ys 
