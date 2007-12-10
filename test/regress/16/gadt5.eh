let

data IntTupleRep t
  = Tup, t = (Int, Int)

in let

tupId :: IntTupleRep t -> t -> t
tupId = \g -> \t ->
          case g of
            Tup -> case t of
                     (a,b) -> (a,b)

in 3
