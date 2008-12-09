{-| Module      :  Messages
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable

	Datatype to represent error messages. One abstraction is the datatype
	MessageBlock, which contains (atomic) pieces of information that are
	reported in the error messages such as types, ranges and code fragments.
-}

module Helium.StaticAnalysis.Messages.Messages where

import Helium.Syntax.UHA
import Helium.Syntax.UHA_Utils
import Helium.Syntax.UHA_Range
import Top.Types 
import Helium.Utils.OneLiner
import Helium.Utils.Similarity (similar)
import Helium.Utils.Utils      (internalError)
import List       (sortBy, sort, partition, union, nub)
import Maybe      (fromJust, isNothing)
import Char       (toUpper)

type Message       = [MessageLine] 

data MessageLine   = MessageOneLiner  MessageBlock
                   | MessageTable     [(Bool, MessageBlock, MessageBlock)]  -- Bool: indented or not
                   | MessageHints     String MessageBlocks

type MessageBlocks = [MessageBlock]               
data MessageBlock  = MessageString       String
                   | MessageRange        Range
                   | MessageType         TpScheme
                   | MessagePredicate    Predicate                                      
                   | MessageOneLineTree  OneLineTree
                   | MessageCompose      MessageBlocks                   

class HasMessage a where
   getRanges            :: a -> [Range]
   getMessage           :: a -> Message  
   
   -- default definitions
   getRanges            _ = []

instance (HasMessage a, HasMessage b) => HasMessage (Either a b) where
   getRanges  = either getRanges  getRanges
   getMessage = either getMessage getMessage

instance Substitutable MessageLine where

   sub |-> ml = case ml of   
                   MessageOneLiner mb -> MessageOneLiner (sub |-> mb)
                   MessageTable table -> MessageTable [ (b, sub |-> mb1, sub |-> mb2) | (b, mb1, mb2) <- table ]
                   MessageHints s mbs -> MessageHints s (sub |-> mbs)

   ftv ml = case ml of
               MessageOneLiner mb -> ftv mb
               MessageTable table -> ftv [ [mb1, mb2] | (_, mb1, mb2) <- table ]
               MessageHints s mbs -> ftv mbs
                                       
instance Substitutable MessageBlock where

   sub |-> mb = case mb of
                   MessageType tp       -> MessageType (sub |-> tp)
                   MessagePredicate p   -> MessagePredicate (sub |-> p)
                   MessageCompose mbs   -> MessageCompose (sub |-> mbs) 
                   _                    -> mb

   ftv mb = case mb of         
               MessageType tp       -> ftv tp
               MessagePredicate p   -> ftv p           
               MessageCompose mbs   -> ftv mbs
               _                    -> []       

-------------------------------------------------------------
-- Smart row constructors for tables

infixl 1 <:>, >:>    -- very low priority

-- do not indent
(<:>) :: String -> MessageBlock -> (Bool, MessageBlock, MessageBlock)
s <:> mb = (False, MessageString s, mb)

-- indented row
(>:>) :: String -> MessageBlock -> (Bool, MessageBlock, MessageBlock)
s >:> mb = (True, MessageString s, mb)

-------------------------------------------------------------
-- Misc

data Entity = TypeSignature
            | TypeVariable
            | TypeConstructor
            | Definition
            | Constructor
            | Variable
            | Import
            | ExportVariable
            | ExportModule
            | ExportConstructor
            | ExportTypeConstructor
            | Fixity
    deriving Eq

sortMessages :: HasMessage a => [a] -> [a]
sortMessages = let f x y = compare (getRanges x) (getRanges y)
               in sortBy f
               
sortNamesByRange :: Names -> Names
sortNamesByRange names =
   let tupleList = [ (name, getNameRange name) | name <- names ]
       (xs,ys)   = partition (isImportRange . snd) tupleList
   in map fst (sortBy (\a b -> snd a `compare` snd b) ys ++ xs)

-- The first argument indicates whether numbers up to ten should be
-- printed "verbose"
ordinal :: Bool -> Int -> String
ordinal b i
    | i >= 1 && i <= 10 && b = table !! (i - 1)
    | i >= 0                 = show i ++ extension i
    | otherwise              = internalError "Messages.hs"
                                             "ordinal"
                                             "can't show numbers smaller than 0"
    where
        table =
            [ "first", "second", "third", "fourth", "fifth", "sixth","seventh"
            , "eighth", "ninth", "tenth"
            ]
        extension i
            | i > 3 && i < 20 = "th"
            | i `mod` 10 == 1 = "st"
            | i `mod` 10 == 2 = "nd"
            | i `mod` 10 == 3 = "rd"
            | otherwise       = "th"
	    
showNumber :: Int -> String
showNumber i | i <= 10 && i >=0 = list !! i
             | otherwise        = show i
   where list = [ "zero", "one", "two", "three", "four", "five"
                , "six", "seven", "eight", "nine", "ten" 
		]
	    
prettyOrList :: [String] -> String
prettyOrList []  = ""
prettyOrList [s] = s
prettyOrList xs  = foldr1 (\x y -> x++", "++y) (init xs) ++ " or "++last xs

prettyAndList :: [String] -> String
prettyAndList []  = ""
prettyAndList [s] = s
prettyAndList xs  = foldr1 (\x y -> x++", "++y) (init xs) ++ " and "++last xs

prettyNumberOfParameters :: Int -> String
prettyNumberOfParameters 0 = "no parameters"
prettyNumberOfParameters 1 = "1 parameter"
prettyNumberOfParameters n = show n++" parameters"

capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs

findSimilar :: Name -> Names -> Names
findSimilar n = filter (\x -> show n `similar` show x)

instance Show Entity where
   show entity = 
      case entity of
         TypeSignature   -> "type signature"
         TypeVariable    -> "type variable"
         TypeConstructor -> "type constructor"
         Definition      -> "definition"
         Constructor     -> "constructor"
         Variable        -> "variable"
         Import          -> "import"
         ExportVariable  -> "exported variable"
         ExportModule    -> "exported module"
         ExportConstructor
                         -> "exported constructor"
         ExportTypeConstructor
                         -> "exported type constructor"
         Fixity          -> "infix declaration"
         _               -> internalError "Messages" "instance Show Entity" "unknown entity"
