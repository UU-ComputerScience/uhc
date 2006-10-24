module EH.Util.ParseUtils
  ( PlainParser, LayoutParser
  
  , parsePlain
  , parseOffsideToResMsgs
  , parseToResMsgs
  
  , parseOffsideToResMsgsStopAtErr
  
  , pAnyFromMap, pAnyKey
  , pMaybe, pMb
  )
  where

import qualified Data.Map as Map
import Data.Maybe
import UU.Parsing
import UU.Parsing.Machine
import UU.Parsing.Offside
import UU.Scanner.Position( Position(..) )

-------------------------------------------------------------------------
-- Type(s) of parsers
-------------------------------------------------------------------------

type LayoutParser tok ep
  = (IsParser (OffsideParser i o tok p) tok,InputState i tok p, OutputState o, Position p)
       => OffsideParser i o tok p ep

type PlainParser tok gp = IsParser p tok => p gp

-------------------------------------------------------------------------
-- Parsing utils
-------------------------------------------------------------------------

valFromPair :: Steps (Pair a (Pair a1 r)) s p -> Steps (a, a1) s p
valFromPair p
  = val fromPair p
  where fromPair (Pair x (Pair y _)) = (x,y)

toResMsgs :: Steps (Pair a r) s pos -> (a, [Message s pos])
toResMsgs steps
  = (r,getMsgs steps)
  where (Pair r _) = evalSteps steps

toOffsideResMsgs :: Steps (a,b) s pos -> (a, [Message s pos])
toOffsideResMsgs steps
  = (r,getMsgs steps)
  where (r,_) = evalSteps steps

parsePlain :: (Symbol s, InputState inp s pos) 
      => AnaParser inp Pair s pos a 
      -> inp 
      -> Steps (a, inp) s pos
parsePlain p inp
  = valFromPair (parse p inp)

parseToResMsgs :: (Symbol s,InputState inp s pos) => AnaParser inp Pair s pos a -> inp -> (a,[Message s pos])
parseToResMsgs p inp
  = toResMsgs (parse p inp)

parseOffsideToResMsgs
  :: (Symbol s, InputState i s p, Position p)
       => OffsideParser i Pair s p a -> OffsideInput i s p -> (a,[Message (OffsideSymbol s) p])
parseOffsideToResMsgs p inp
  = toOffsideResMsgs (parseOffside p inp)

-------------------------------------------------------------------------
-- Parsing, stopping at first error
-------------------------------------------------------------------------

handleEofStopAtErr input
  = case splitStateE input
       of Left'  s  ss  ->  NoMoreSteps (Pair ss ())
          Right' ss     ->  NoMoreSteps (Pair ss ())

parseStopAtErr
  :: (Symbol s, InputState inp s pos) 
      => AnaParser inp Pair s pos a 
      -> inp 
      -> Steps (Pair a (Pair inp ())) s pos
parseStopAtErr
  = parsebasic handleEofStopAtErr

parseOffsideStopAtErr
  :: (Symbol s, InputState i s p, Position p) 
     => OffsideParser i Pair s p a 
     -> OffsideInput i s p
     -> Steps (a, OffsideInput i s p) (OffsideSymbol s) p
parseOffsideStopAtErr (OP p) inp
  = valFromPair (parseStopAtErr p inp)

parseOffsideToResMsgsStopAtErr
  :: (Symbol s, InputState i s p, Position p) =>
     OffsideParser i Pair s p a
     -> OffsideInput i s p
     -> (a, [Message (OffsideSymbol s) p])
parseOffsideToResMsgsStopAtErr p inp
  = toOffsideResMsgs (parseOffsideStopAtErr p inp)

-------------------------------------------------------------------------
-- Misc combinators
-------------------------------------------------------------------------

-- parse possibly present p
pMaybe :: (IsParser p s) => a1 -> (a -> a1) -> p a -> p a1
pMaybe n j p = j <$> p <|> pSucceed n

pAnyKey :: (IsParser p s) => (a1 -> p a) -> [a1] -> p a
pAnyKey pKey = foldr1 (<|>) . map pKey

pMb :: (IsParser p s) => p a -> p (Maybe a)
pMb = pMaybe Nothing Just

-- given (non-empty) key->value map, return parser for all keys returning corresponding value
pAnyFromMap :: (IsParser p s) => (k -> p a1) -> Map.Map k v -> p v
pAnyFromMap pKey m = foldr1 (<|>) [ v <$ pKey k | (k,v) <- Map.toList m ]

