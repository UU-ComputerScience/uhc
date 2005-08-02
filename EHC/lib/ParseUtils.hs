module ParseUtils where

import UU.Parsing
import qualified Data.Map as Map

-------------------------------------------------------------------------
-- Parsing utils
-------------------------------------------------------------------------

parseToResMsgs :: (Symbol s,InputState inp s pos) => AnaParser inp Pair s pos a -> inp -> (a,[Message s pos])
parseToResMsgs p inp
  = (r,getMsgs steps)
  where steps = parse p inp
        (Pair r _) = evalSteps steps

-- given (non-empty) key->value map, return parser for all keys returning corresponding value
pAnyFromMap :: (IsParser p s) => (k -> p a1) -> Map.Map k v -> p v
pAnyFromMap pKey m = foldr1 (<|>) [ v <$ pKey k | (k,v) <- Map.toList m ]

-- parse possibly present p
pMaybe :: (IsParser p s) => a1 -> (a -> a1) -> p a -> p a1
pMaybe n j p = j <$> p <|> pSucceed n