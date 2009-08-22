-- merge of fundefs into case

data X = XX Int | YY

f (XX a)  YY    = a
f (XX b) (XX c) = c
f  YY    (XX d) = d
f  YY     YY    = 3

main = f YY (XX 333)
