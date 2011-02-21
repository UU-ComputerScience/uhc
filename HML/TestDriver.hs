{-# LANGUAGE  FlexibleInstances,
              TypeSynonymInstances,
              MultiParamTypeClasses,
              ScopedTypeVariables, 
              CPP  #-}

module TestDriver where

import Data.Char
import Data.Typeable

import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Derived
import System.IO
import GHC.IO.Handle.Types

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

type ID      = Int
type Value   = String
type Message = String

data Header  
 = Header { hid    :: ID
          , hvalue :: Value
          }
             deriving Show

data Payload = Echo Message
             | Test Command
             deriving Show
             
data TySig  
 = TySig { sigValue  :: Value
         , sigResult :: Result
         }
            deriving Show
            
data Result = OK (Maybe String)
            | FAILED
            deriving (Eq, Show)
             
data Command 
 = Command { cmdId    :: ID
           , command  :: Message
           , headers  :: [ID]
           , payloads :: [ID]
           , typeSigs :: [TySig]
           , body     :: Value
           , result   :: Result
           } deriving Show
           
data TestFile
 = TestFile { heads :: [Header]
            , loads :: [Payload]
            } deriving Show
            
string :: String -> Parser String
string = pToken
pKey keyw = pToken keyw `micro` 1 <* spaces
pParens p  = pSym '(' *> p <* pSym ')'
pBracks p  = pSym '[' *> p <* pSym ']'
pCurlys p  = pSym '{' *> p <* pSym '}'
pLiteral p = pSym '"' *> p <* pSym '"'

pSatisfy :: ( Char -> Bool ) -> Parser Char
pSatisfy = pSym

instance (Typeable a, Typeable b) => Show (a -> b) where
        show e = '\\' : (show . typeOf) e

instance (Typeable a, Show a, loc `IsLocationUpdatedBy` a) => Provides (Str a loc) (a -> Bool) a where
    splitState f = splitState (f, show f , undefined :: a   )

-- parsing numbers

pDigitAsInt = digit2Int <$> pDigit 
pNatural = foldl (\a b -> a * 10 + b ) 0 <$> pList1 pDigitAsInt
digit2Int a =  ord a - ord '0'

-- parsing letters and identifiers

pAscii = pSym (chr 0, chr 255)
pDigit = pSym ('0', '9')
pString = pSym '"' *> pFail
      <<|> (\a b c->a:b:c) <$> pSym '\\' <*> pAscii <*> pString
      <<|> (:) <$> pAscii <*> pString
       <|> pReturn []
            
pManyTill :: P st a -> P st b -> P st [a]
pManyTill p end = [] <$ end 
                  <<|> 
                  (:) <$> p <*> pManyTill p end
            
spaces :: Parser String
spaces = pMunch (`elem` " \n")
            
pHeader :: Parser Header
pHeader =  Header
       <$> (pKey "-- >" *> spaces *> pNatural <* pKey "\n")
       <*> pManyTill pAscii (pKey "-- <" *> spaces *> pNatural *> spaces)
       
pEcho :: Parser Payload 
pEcho = Echo <$> (pKey "-- &" *> spaces *> pManyTill pAscii (string "\n"))

pTest :: Parser Payload
pTest = Test <$> pCommand

pPayload :: Parser Payload
pPayload = spaces *> (pEcho <|> pTest) <* spaces

pList' :: Parser a -> Parser [a]
pList' pa = (:) <$> pa <*> pList' pa
        <<|> pReturn []
         
pMaybe' :: Parser [a] -> Parser (Maybe [a])
pMaybe' pa  =  (\a -> if null a then Nothing else Just a) <$> pa
           <|> pReturn Nothing

pCommand :: Parser Command
pCommand = Command 
        <$> (pKey "-- #" *> pNatural)      <* spaces
        <*> pLiteral pString               <* spaces
        <*> pCurlys (pSep "," pNatural)    <* spaces
        <*> pBracks (pSep "," pNatural)    <* spaces
        <*> pList' pTySig                  <* spaces
        <*> pManyTill pAscii (pKey "-- #") <* spaces
        <*> pResult
        
pSep :: String -> Parser a -> Parser [a]
pSep p pa = (:) <$> pa <* spaces <* string p <* spaces <*> pSep p pa
         <|> return <$> pa
        <<|> pReturn []
               
pTySig :: Parser TySig
pTySig =  TySig <$> (string "-- @" *> spaces *> pManyTill pAscii (pKey "-- @"))
                <*> (spaces *> pResult)

        
pResult :: Parser Result
pResult =  FAILED 
       <$  string "FAIL"
       <|> OK <$> (string "OK" *> spaces *> pMaybe' (pLiteral pString))
       
pTestFile :: Parser TestFile
pTestFile = TestFile 
         <$> pList' pHeader
         <*> pList' pPayload
         
run :: Show t =>  Parser t -> String -> IO t
run p inp = do  inp' <- readFile inp
                let r@(a, errors) =  parse ( (,) <$> p <*> pEnd) (listToStr inp' (0,0))
                unless (null errors) $ do putStr ("-- > Correcting steps: \n")
                                          show_errors errors
                                          -- error "Parse Failed."
                return a

parseTest :: String -> IO TestFile
parseTest file = run pTestFile file

test = fmap loads $ parseTest "TestMod.hs"
