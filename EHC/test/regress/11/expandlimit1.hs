-- should report error on 'too far' expansion of 'E x' when fitting: Int <= E x
type E x = E x

f :: E x -> E x
f x = x

v1 = f 3
main = v1
