let data Error = UserError Int | OverFlow | DivByZero
in
let foreign import jazy "primThrow" throw :: Error -> a
in throw (UserError 5)

