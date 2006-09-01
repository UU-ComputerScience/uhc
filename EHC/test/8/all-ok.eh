let id = \x -> x
    const = \x -> \x' -> x
    v = id 3
    v' = const v v
    data L a = N | C a (L a)
 in v'

