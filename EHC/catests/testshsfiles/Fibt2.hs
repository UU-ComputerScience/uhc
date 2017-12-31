fibt :: Int -> Int

fibt x = 
  let u1 = x < 1
  in case u1 of 
    True -> 0
    False -> 
      let u2 = x == 1
      in case u2 of
        True -> 1
        False ->
          let 
            u3 = x-1
            u4 = x-2
          in 
            let
              f1 = fibt u3
              f2 = fibt u4
            in f1 + f2

main = print (fibt 30)