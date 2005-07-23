module ParseUtils where

import UU.Parsing

-------------------------------------------------------------------------
-- Parsing utils
-------------------------------------------------------------------------

parseToResMsgs :: (Symbol s,InputState inp s pos) => AnaParser inp Pair s pos a -> inp -> (a,[Message s pos])
parseToResMsgs p inp
  = (r,getMsgs steps)
  where steps = parse p inp
        (Pair r _) = evalSteps steps

