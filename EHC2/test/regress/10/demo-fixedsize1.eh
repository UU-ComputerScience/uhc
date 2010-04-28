let  r   =   (i = 3, c = 'x', id = \x->x)
     s   =   (r | c := 5)
in   let  v = (r#id r#i, r#id r#c)
          vi = v#1
     in   vi
