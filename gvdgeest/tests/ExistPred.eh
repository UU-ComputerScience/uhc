let data Bool = True | False

    class ToBool a where
      toBool :: a -> Bool
 
    instance ToBool Bool where
      toBool = \x -> x

    instance ToBool Int where
      toBool = \x -> True
in 
let i :: exists a . ToBool a => a
    i = 1

in toBool i
  
   
   
    

     
