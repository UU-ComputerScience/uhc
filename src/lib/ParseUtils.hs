module ParseUtils where

import qualified Data.Map as Map
import Data.Maybe
import UU.Parsing
-- import UUTest.Parsing.Offside
import UU.Parsing.Offside
import UU.Scanner.Position( Position(..) )

-------------------------------------------------------------------------
-- Parsing utils
-------------------------------------------------------------------------

parseToResMsgs :: (Symbol s,InputState inp s pos) => AnaParser inp Pair s pos a -> inp -> (a,[Message s pos])
parseToResMsgs p inp
  = (r,getMsgs steps)
  where steps = parse p inp
        (Pair r _) = evalSteps steps

parseOffsideToResMsgs
  :: (Symbol s, InputState i s p, Position p)
       => OffsideParser i Pair s p a -> OffsideInput i s p -> (a,[Message (OffsideSymbol s) p])
parseOffsideToResMsgs p inp
  = (r,getMsgs steps)
  where steps = parseOffside p inp
        (r,_) = evalSteps steps

-- given (non-empty) key->value map, return parser for all keys returning corresponding value
pAnyFromMap :: (IsParser p s) => (k -> p a1) -> Map.Map k v -> p v
pAnyFromMap pKey m = foldr1 (<|>) [ v <$ pKey k | (k,v) <- Map.toList m ]

-- parse possibly present p
pMaybe :: (IsParser p s) => a1 -> (a -> a1) -> p a -> p a1
pMaybe n j p = j <$> p <|> pSucceed n

pAnyKey :: (IsParser p s) => (a1 -> p a) -> [a1] -> p a
pAnyKey pKey = foldr1 (<|>) . map pKey

pMb :: (IsParser p s) => p a -> p (Maybe a)
pMb = pMaybe Nothing Just