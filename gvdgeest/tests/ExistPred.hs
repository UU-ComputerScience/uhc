

data Test = forall a . Eq a => Test a


x = Test True

f (Test x) = x == x
