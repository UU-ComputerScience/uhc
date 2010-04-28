-- test for combi of lambda + operator expression

x ++ y = x
id x = x
x1 p q = id (\inp -> p inp ++ q inp )
x2 p q = id (\inp -> (p inp ++ q inp) )
