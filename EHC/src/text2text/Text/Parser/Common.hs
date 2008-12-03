-------------------------------------------------------------------------
-- Parser common stuff
-------------------------------------------------------------------------

module Text.Parser.Common
  ( module UU.Parsing
  , module EH.Util.ParseUtils
  , module EH.Util.ScanUtils

  , T2TPr, T2TPr', T2TPr2, T2TPr2', T2TPr3'
  
  , pBegContent, pEndContent
  , pKey, pCmd, pText, pVar
  , pWhite, pNl
  , pAST
  
  , scan
  , ScanOptsMp
  , ScState(..), defaultScState
  
  , ScInput(..)
  , ScType(..)
  
  )
  where

import Data.Char
import qualified Data.Map as Map
import qualified Data.Set as Set

import UU.Parsing
-- import UU.Scanner.Position( initPos, Pos )

import EH.Util.ScanUtils
import EH.Util.ParseUtils
import EH.Util.Utils

import Common
import Text

-------------------------------------------------------------------------------------------
-- Scanning preliminaries
-------------------------------------------------------------------------------------------

-- what are we scanning?
data ScInput
  = ScInput_Uninterpreted		String		-- plain, yet uninterpreted text
  | ScInput_TextAST				TextItems	-- already Text AST

data ScType
  = ScTpMeta				-- text blocks
  | ScTpMetaMeta			-- text blocks meta info
  | ScTpContent TextType	-- text content
  deriving (Eq,Ord,Show)

data ScState
  = ScState { scstateLevel	:: Int 			-- level
            , scstateType	:: ScType		-- text type
            }
  | ScSkip
  deriving (Eq,Ord,Show)

defaultScState :: ScState
defaultScState = ScState 0 ScTpMeta

type ScanOptsMp = Map.Map ScType ScanOpts

data TokKind
  = TkBegContent  | TkEndContent
  | TkReserved | TkCmd
  | TkNl   | TkEOF | TkErr
  | TkText | TkInt | TkStr | TkWhite
  deriving (Show,Eq,Ord)

data Tok
  = Tok { tokKind :: TokKind, tokStr :: String, tokPos :: InFilePos, tokState :: ScState }
  | TokAST { tokAST :: TextItems }

instance Eq Tok where
  (Tok k1 b1 _ _) == (Tok k2 b2 _ _) = k1 == k2 && (k1 `notElem` [TkReserved,TkCmd] || b1 == b2)
  (TokAST _     ) == (TokAST _     ) = True
  _               == _               = False

instance Ord Tok where
  (Tok k1 b1 _ _) `compare` (Tok k2 b2 _ _)
    = if ck == EQ
      then if k1 `elem` [TkReserved,TkCmd] then b1 `compare` b2 else ck
      else ck
    where ck = k1 `compare` k2
  (TokAST _     ) `compare` (TokAST _     ) = EQ
  (TokAST _     ) `compare` (Tok _  _  _ _) = LT
  (Tok _  _  _ _) `compare` (TokAST _     ) = GT

instance Show Tok where
  show t@(Tok _ _ _ _) = show (tokPos t) ++ show (tokStr t) ++ show (tokKind t)
  show t@(TokAST _   ) = "AST"

instance Symbol Tok

-------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------

scan :: ScanOptsMp -> ScState -> [ScInput] -> [Tok]
scan scoMp st sci
  = takeWhile ((/=TkEOF) . tokKind) (sc infpStart st "" sci)
  where -- high level
        sc p st              "" (ScInput_Uninterpreted i : sci)
                                                    = sc p st i sci
        sc p st              "" (ScInput_TextAST ast : sci)
                                                    = TokAST ast : sc p st "" sci
        
        -- EOF
        sc p st              "" []                  = [Tok TkEOF "" p st]
        
        -- text
        sc p@(InFilePos _ 1) st@(ScState l ScTpMeta) ('@':'@':'[':s') sci
                                                    = Tok TkBegContent   "@@["  p st  : sc (infpAdvCol 3 p) (ScState (l+1) ScTpMetaMeta) s' sci
        sc p@(InFilePos _ 1) st@(ScState l ScTpMeta) ('@':'@':']':s') sci
                                                    = Tok TkEndContent   "@@]"  p st  : sc (infpAdvCol 3 p) (ScState (l-1) ScTpMetaMeta) s' sci
        sc p st@(ScState l ScTpMetaMeta) s@(c:s') sci
          | isLF c                                  = sc (infpAdvLine p) (ScState l ScTpMeta) s' sci
        sc p st@(ScState _ sctp@(ScTpContent _)) s@(c:s') sci
          | isLF c                                  = Tok TkNl [c] p st : sc (infpAdvLine p) st s' sci
        sc p st@(ScState _ sctp@(ScTpContent TextType_DocLaTeX)) ('\\':s@(c:_)) sci
          | isVarStart c                            = scKw isVarStart isCmd TkCmd (infpAdvCol 1 p) st s sci
        sc p st@(ScState _ sctp) s@(c:s') sci
          | isSpec sctp c                           = Tok TkReserved   [c] p st : sc (infpAdvCol 1 p) st s' sci
        sc p st@(ScState _ sctp@ScTpMetaMeta) s@(c:_) sci
          | isVarStart c                            = scKw isVarRest isKeyw TkReserved p st s sci
        sc p st@(ScState _ sctp@(ScTpContent _)) s@(c:_) sci
          | isVarStart c                            = scKw isVarRest isKeyw TkReserved p st s sci
{-
        sc p st@(ScState _ ScTpMetaMeta) s@(c:_) sci
          | isDigit c                               = Tok TkInt        w   p st : sc (infpAdvStr w p) st s' sci
                                                    where (w,s') = span isDigit s
        sc p st@(ScState _ ScTpMetaMeta) s@(c:s') sci
          | isStrQuote c                            = Tok TkStr        w   p st : sc (infpAdvStr w . infpAdvCol 2 $ p) st s3 sci
                                                    where (w,s2) = span isStr s'
                                                          s3 = case s2 of
                                                                 (c:s) | isStrQuote c -> s
                                                                 _                    -> s2
        sc p st@(ScState _ sctp@ScTpMetaMeta) s@(c:_) sci
          | isOpch sctp c                           = scKw (isOpch sctp) p st s sci
-}
        sc p st@(ScState _ ScTpMeta) s@(c:s') sci
          | isLF c                                  = Tok TkText       ""  p st : sc (infpAdvLine p) st s' sci
          | otherwise                               = Tok TkText       b   p st : sc p' st s'' sci
                                                    where (b,p',s'') = case break isLF s of
                                                                         (b,(c:s)) | isLF c -> (b,infpAdvLine p,s)
                                                                         (b,s)              -> (b,infpAdvStr b p,s)
        sc p st@(ScState _ sctp@(ScTpContent TextType_DocLaTeX)) s@(c:s') sci
          | isWhite c                               = Tok TkWhite w p st : sc (infpAdvStr w p) st s'' sci
                                                    where (w,s'') = span isWhite s
        sc p st@(ScState _ sctp@(ScTpContent TextType_DocLaTeX)) s@(c:s') sci
          | isBlack c                               = Tok TkText b p st : sc (infpAdvStr b p) st s'' sci
                                                    where (b,s'') = break (\c -> isWhite c || isSpec sctp c || isLF c) s
        sc p st             s@(c:s') sci            = Tok TkErr        [c] p st : sc (infpAdvCol 1 p) st s' sci
{-
        sc p st@(ScState _ ScTpMeta) s@(c:s')
          | isLF c                                  = Tok TkNl         [c] p st : sc (infpAdvLine p) st s'
          | otherwise                               = Tok TkText       b   p st : sc (infpAdvStr b p) st s''
                                                    where (b,s'') = break isLF s
-}
{-
  where sc p st@(ScLexMeta l) ('%':'%':'}':s')      = Tok TkEndExpand  "%%}"  p st : sc (ai 3 p) (ScChunk l) s'
        sc p st@(ScLexMeta _) s@(c:_)
          | isWhite c                               = sc (a w p) st s'
                                                    where (w,s') = span isWhite s
        sc p st               s@(c:s')
          | isLF c                                  = Tok TkNl         [c] p st : sc (al p) st' s'
                                                    where st' = case st of
                                                                  ScLexMeta l -> ScChunk l
                                                                  _           -> st
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':'[':'[':s')
                                                    = Tok TkBegGroup   "%%[[" p st : sc (ai 4 p) (ScLexMeta (l+1)) s'
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':']':'[':s')
                                                    = Tok TkElseGroup  "%%][" p st : sc (ai 4 p) (ScLexMeta (l)) s'
        sc p@(InFilePos _ 1) ScSkip            ('%':'%':'[':s')
                                                    = Tok TkBegChunk   "%%["  p ScSkip  : sc (ai 3 p) (ScLexMeta 0) s'
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':']':']':s')
          | l >  0                                  = Tok TkEndChunk   "%%]]" p st : sc (ai 4 p) (ScChunk (l-1)) s'
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':']':s')
          | l == 0                                  = Tok TkEndChunk   "%%]"  p st : sc (ai 3 p) ScSkip s'
          | l >  0                                  = Tok TkEndChunk   "%%]"  p st : sc (ai 3 p) (ScChunk (l-1)) s'
        sc p@(InFilePos _ _) st@(ScChunk l)    ('%':'%':'@':'[':s')
                                                    = Tok TkBegInline  "%%@[" p st : sc (ai 4 p) (ScInline l) s'
        sc p@(InFilePos _ _) st@(ScChunk l)    ('%':'%':'@':'{':s')
                                                    = Tok TkBegExpand  "%%@{" p st : sc (ai 4 p) (ScLexMeta l) s'
        sc p@(InFilePos _ _) st@(ScChunk l)    ('%':'%':'%':s)
                                                    = Tok TkText       b'     p st : sc (ai (1 + length b') p) (ScChunk l) s'
                                                    where (b,s') = span isBlack s
                                                          b'     = "%%" ++ b
        sc p@(InFilePos _ 1) st@(ScChunk l)    ('%':'%':'@':s')
                                                    = Tok TkNameRef    "%%@"  p st : sc (ai 3 p) (ScLexMeta l) s'
        sc p@(InFilePos _ _) st@(ScInline l)   ('%':'%':']':s')
                                                    = Tok TkEndInline  "%%]"  p st : sc (ai 3 p) (ScChunk l) s'
        sc p@(InFilePos _ _) st@(ScInline l)   s       = Tok TkText       il     p st : sc (a il p) (ScInline l) s'
                                                    where (il,s') = spanInline s
                                                          spanInline s@('%':'%':']':_) = ("",s)
                                                          spanInline ""                = ("","")
                                                          spanInline (c:s)             = let (b,e) = spanInline s in (c:b,e)
        sc p st             s@(c:s')                = sc (ai 1 p) st s'
-}

        scKw f isKeyCmd keytok p st@(ScState _ sctp) s sci   = Tok tk w p st : sc (infpAdvStr w p) st s' sci
                                                    where (w,s') = span f s
                                                          tk = if isKeyCmd sctp w then keytok else TkText

        opt st p                                    = maybe False p $ Map.lookup st scoMp
        isSpec st c                                 = opt st (\o -> c `Set.member` scoSpecChars o)
        isOpch st c                                 = opt st (\o -> c `Set.member` scoOpChars o)
        isKeyw st w                                 = opt st (\o -> w `Set.member` scoKeywordsTxt o)
        isCmd  st w                                 = opt st (\o -> w `Set.member` scoCommandsTxt o)

-------------------------------------------------------------------------
-- Parsers directly related to scanning
-------------------------------------------------------------------------

type T2TPr   c        = (IsParser p Tok) => p c
type T2TPr'  c        = (IsParser p Tok) => c -> p c
type T2TPr2  c        = (IsParser p Tok) => p c -> p c
type T2TPr2' c1 c2    = (IsParser p Tok) => p c1 -> p c2
type T2TPr3  c        = (IsParser p Tok) => p c -> p c -> p c
type T2TPr3' c1 c2 c3 = (IsParser p Tok) => p c1 -> p c2 -> p c3

pBegContent, pEndContent
  , pNl :: T2TPr Tok
pBegContent   = pSym (Tok TkBegContent  "@@["  infpNone ScSkip)
pEndContent   = pSym (Tok TkEndContent  "@@]"  infpNone ScSkip)
pNl           = pSym (Tok TkNl          "LF"   infpNone ScSkip)

pKey :: T2TPr' String
pKey k = tokStr <$> pSym (Tok TkReserved k infpNone ScSkip)

pCmd :: T2TPr' String
pCmd k = tokStr <$> pSym (Tok TkCmd k infpNone ScSkip)

pVar :: T2TPr String
pVar = tokStr <$> pSym (Tok TkText "<ident>" infpNone ScSkip)

pInt :: T2TPr String
pInt = tokStr <$> pSym (Tok TkInt "0" infpNone ScSkip)

pInt' :: T2TPr Int
pInt' = strToInt <$> pInt

pStr :: T2TPr String
pStr = tokStr <$> pSym (Tok TkStr "<string>" infpNone ScSkip)

pText :: T2TPr String
pText = tokStr <$> pSym (Tok TkText "<text>" infpNone ScSkip)

pWhite :: T2TPr String
pWhite = tokStr <$> pSym (Tok TkWhite "<whitespace>" infpNone ScSkip)

pAST :: T2TPr TextItems
pAST = tokAST <$> pSym (TokAST undefined)




