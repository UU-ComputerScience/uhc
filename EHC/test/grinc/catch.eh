let data Error = UserError Int | OverFlow | DivByZero | Undefined
in
let foreign import jazy "primThrow" throw :: Error -> a
    foreign import jazy "primCatch" catch :: Int -> (Error -> Int) -> Int
    undefined = throw Undefined
in catch (throw (UserError 5)) (\e -> case e of (UserError n) -> n)
                     

