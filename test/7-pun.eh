let  r = (p = 3, q = 'x')
in   let  v1 =  case r of
                  (p = a, q = b) -> a
          v2 =  case r of
                  (p, q=b) -> p
          v3 =  case (3,'x') of
                  (p, q) -> p
          v4 =  case r of
                  (p, q) -> p
          v5 =  case (3,'x') of
                  (1=p, q) -> p
     in   v1
