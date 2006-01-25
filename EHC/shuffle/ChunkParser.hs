-------------------------------------------------------------------------
-- Chunk Parser
-------------------------------------------------------------------------

module ChunkParser
  where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import IO
import UU.Parsing
import UU.Parsing.CharParser
import UU.Scanner.Position( initPos, Pos )
import ParseUtils
import Common
import Utils (wordsBy)
import MainAG

-------------------------------------------------------------------------------------------
-- Scanning
-------------------------------------------------------------------------------------------

data ScanOpts
  =  ScanOpts
        {   scoKeywordsTxt      ::  Set.Set String
        ,   scoSpecChars        ::  Set.Set Char
        ,   scoOpChars          ::  Set.Set Char
        }

type ScanOptsMp = Map.Map ScState ScanOpts

chKindMp = Map.fromList [ ("hs",ChHS), ("ag",ChAG), ("plain",ChPlain) ]
chDestMp = Map.fromList [ ("here",ChHere), ("hide",ChHide) ]
chWrapMp = Map.fromList [ ("code",ChWrapCode), ("safecode",ChWrapBoxCode), ("boxcode",ChWrapBoxCode), ("tt",ChWrapTT), ("tttiny",ChWrapTTtiny) ]

kwTxtAsVarTooA
  = [ "module", "import", "export", "wrap" ]
    ++ Map.keys chKindMp

kwTxtAsVarTooB
  = kwTxtAsVarTooA
    ++ Map.keys chDestMp
    ++ Map.keys chWrapMp

shuffleScanOpts :: ScanOptsMp
shuffleScanOpts
  = Map.fromList
        [ ( ScLexMeta 0
          , ScanOpts
              { scoKeywordsTxt      =   Set.fromList (kwTxtAsVarTooB ++ [ "_", "-", ".", "<", "=" ])
              , scoSpecChars        =   Set.fromList "(),"
              , scoOpChars          =   Set.fromList "+-=*&^%$#@!\\|><~`;:?/_."
              }
          )
        ]

data ScState
  = ScChunk Int | ScLexMeta Int | ScInline Int | ScSkip
  deriving Show

instance Eq ScState where
  ScChunk   _ == ScChunk   _ = True
  ScLexMeta _ == ScLexMeta _ = True
  ScInline  _ == ScInline  _ = True
  ScSkip      == ScSkip      = True
  _           == _           = False

instance Ord ScState where
  ScChunk   _ < ScLexMeta _ = True
  ScChunk   i < s           = ScLexMeta i < s
  ScLexMeta _ < ScInline  _ = True
  ScLexMeta i < s           = ScInline  i < s
  ScInline  _ < ScSkip      = True
  _           < _           = False

data TokPos
  = TokPos { tkpLine, tkpColumn :: Int }
  deriving (Eq,Ord)

instance Show TokPos where
  show (TokPos l c) = if l < 0 || c < 0 then "" else "(" ++ show l ++ ":" ++ show c ++ ")"

tkpStart :: TokPos
tkpStart = TokPos 1 1

tkpNone :: TokPos
tkpNone = TokPos (-1) (-1)

data TokKind
  = TkBegChunk  | TkEndChunk
  | TkBegInline | TkEndInline
  | TkBegGroup
  | TkNameRef
  | TkReserved
  | TkNl   | TkEOF
  | TkText | TkInt | TkStr
  deriving (Show,Eq,Ord)

data Tok
  = Tok { tokKind :: TokKind, tokWhite :: String, tokBlack :: String, tokPos :: TokPos, tokState :: ScState }

instance Eq Tok where
  (Tok k1 _ b1 _ _) == (Tok k2 _ b2 _ _) = k1 == k2 && (k1 /= TkReserved || b1 == b2)

instance Ord Tok where
  (Tok k1 _ b1 _ _) `compare` (Tok k2 _ b2 _ _)
    = if ck == EQ
      then if k1 == TkReserved then b1 `compare` b2 else ck
      else ck
    where ck = k1 `compare` k2

instance Show Tok where
  show t = show (tokPos t) ++ show (tokBlack t)

instance Symbol Tok

mbTokWhite :: Tok -> Maybe String
mbTokWhite (Tok _ w _ _ _) = if null w then Nothing else Just w

isVarStart :: Char -> Bool
isVarStart c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z'

isVarRest :: Char -> Bool
isVarRest c = isVarStart c || isDigit c || c `elem` "'_"

isWhite :: Char -> Bool
isWhite = (`elem` " \t")

{-
isDig :: Char -> Bool
isDig c = c >= '0' && c <= '9'
-}

isLF :: Char -> Bool
isLF = (`elem` "\n\r")

isBlack :: Char -> Bool
isBlack c = not (isWhite c || isLF c)

isStrQuote :: Char -> Bool
isStrQuote c = c == '"'

isStr :: Char -> Bool
isStr c = not (isStrQuote c || isLF c)

str2int :: String -> Int
str2int = foldl (\i c -> i * 10 + ord c - ord '0') 0

scan :: ScanOptsMp -> ScState -> String -> [Tok]
scan scoMp st s
  = takeWhile ((/=TkEOF) . tokKind) (sc tkpStart st s)
  where sc p st               ""                      = [Tok TkEOF "" "" p st]
        sc p st@(ScChunk _)   s@(c:_)
          | isWhite c                               = t {tokWhite = w} : ts
                                                    where (w,s') = span isWhite s
                                                          (t:ts) = sc (a w p) st s'
        sc p st@(ScLexMeta _) s@(c:_)
          | isWhite c                               = sc (a w p) st s'
                                                    where (w,s') = span isWhite s
        sc p st               s@(c:s')
          | isLF c                                  = Tok TkNl       ""   [c] p st : sc (al p) st' s'
                                                    where st' = case st of
                                                                  ScLexMeta l -> ScChunk l
                                                                  _           -> st
        sc p st@(ScLexMeta _) s@(c:s')
          | isSpec st c                             = Tok TkReserved ""   [c] p st : sc (ai 1 p) st s'
        sc p st@(ScLexMeta _) s@(c:_)
          | isVarStart c                            = scKw isVarRest p st s
        sc p st@(ScLexMeta _) s@(c:_)
          | isDigit c                               = Tok TkInt      ""   w   p st : sc (a w p) st s'
                                                    where (w,s') = span isDigit s
        sc p st@(ScLexMeta _) s@(c:s')
          | isStrQuote c                            = Tok TkStr      ""   w   p st : sc (a w . ai 2 $ p) st s3
                                                    where (w,s2) = span isStr s'
                                                          s3 = case s2 of
                                                                 (c:s) | isStrQuote c -> s
                                                                 _                    -> s2
        sc p st@(ScLexMeta _) s@(c:_)
          | isOpch st c                             = scKw (isOpch st) p st s
        sc p@(TokPos _ 1) st@(ScChunk l)    ('%':'%':'[':'[':s')
                                                    = Tok TkBegGroup   "" "%%[[" p st : sc (ai 4 p) (ScLexMeta (l+1)) s'
        sc p@(TokPos _ 1) ScSkip            ('%':'%':'[':s')
                                                    = Tok TkBegChunk   "" "%%["  p ScSkip  : sc (ai 3 p) (ScLexMeta 0) s'
        sc p@(TokPos _ 1) st@(ScChunk l)    ('%':'%':']':s')
          | l == 0                                  = Tok TkEndChunk   "" "%%]"  p st : sc (ai 3 p) ScSkip s'
          | l >  0                                  = Tok TkEndChunk   "" "%%]"  p st : sc (ai 3 p) (ScChunk (l-1)) s'
        sc p@(TokPos _ _) st@(ScChunk l)    ('%':'%':'@':'[':s')
                                                    = Tok TkBegInline  "" "%%@[" p st : sc (ai 4 p) (ScInline l) s'
        sc p@(TokPos _ 1) st@(ScChunk l)    ('%':'%':'%':s)
                                                    = Tok TkText       "" b'     p st : sc (ai (1 + length b') p) (ScChunk l) s'
                                                    where (b,s') = span isBlack s
                                                          b'     = "%%" ++ b
        sc p@(TokPos _ 1) st@(ScChunk l)    ('%':'%':'@':s')
                                                    = Tok TkNameRef    "" "%%@"  p st : sc (ai 3 p) (ScLexMeta l) s'
        sc p@(TokPos _ _) st@(ScInline l)   ('%':'%':']':s')
                                                    = Tok TkEndInline  "" "%%]"  p st : sc (ai 3 p) (ScChunk l) s'
        sc p@(TokPos _ _) st@(ScInline l)   s       = Tok TkText       "" il     p st : sc (a il p) (ScInline l) s'
                                                    where (il,s') = spanInline s
                                                          spanInline s@('%':'%':']':_) = ("",s)
                                                          spanInline ""                = ("","")
                                                          spanInline (c:s)             = let (b,e) = spanInline s in (c:b,e)
        sc p st@(ScChunk _) s@(c:_)
          | isBlack c                               = Tok TkText       "" b      p st : sc (a b p) st s'
                                                    where (b,s') = span isBlack s
        sc p st             s@(c:s')                = sc (ai 1 p) st s'
        scKw f p st s                               = Tok tk           "" w      p st : sc (a w p) st s'
                                                    where (w,s') = span f s
                                                          tk = if isKeyw st w then TkReserved else TkText
        a s                                         = ai (length s)
        ai i p                                      = p {tkpColumn = i + tkpColumn p}
        al   p                                      = p {tkpLine = 1 + tkpLine p, tkpColumn = 1}
        opt st p                                    = maybe False p $ Map.lookup st scoMp
        isSpec st c                                 = opt st (\o -> c `Set.member` scoSpecChars o)
        isOpch st c                                 = opt st (\o -> c `Set.member` scoOpChars o)
        isKeyw st w                                 = opt st (\o -> w `Set.member` scoKeywordsTxt o)

pBegChunk, pEndChunk, pBegInline, pEndInline, pBegGroup, pBegNameRef, pNl :: (IsParser p Tok) => p Tok
pBegChunk   = pSym (Tok TkBegChunk  "" "%%["  tkpNone ScSkip)
pEndChunk   = pSym (Tok TkEndChunk  "" "%%]"  tkpNone ScSkip)
pBegInline  = pSym (Tok TkBegInline "" "%%@[" tkpNone ScSkip)
pEndInline  = pSym (Tok TkEndInline "" "%%]"  tkpNone ScSkip)
pBegGroup   = pSym (Tok TkBegGroup  "" "%%[[" tkpNone ScSkip)
pBegNameRef = pSym (Tok TkNameRef   "" "%%@"  tkpNone ScSkip)
pNl         = pSym (Tok TkNl        "" "LF"   tkpNone ScSkip)

pKey :: (IsParser p Tok) => String -> p String
pKey k = tokBlack <$> pSym (Tok TkReserved "" k tkpNone ScSkip)

pVar :: (IsParser p Tok) => p String
pVar = tokBlack <$> pSym (Tok TkText "" "<ident>" tkpNone ScSkip)

pInt :: (IsParser p Tok) => p String
pInt = tokBlack <$> pSym (Tok TkInt "" "0" tkpNone ScSkip)

pInt' :: (IsParser p Tok) => p Int
pInt' = str2int <$> pInt

pStr :: (IsParser p Tok) => p String
pStr = tokBlack <$> pSym (Tok TkStr "" "<string>" tkpNone ScSkip)

pText :: (IsParser p Tok) => p Tok
pText = pSym (Tok TkText "" "<text>" tkpNone ScSkip)

pWhiteBlack :: (IsParser p Tok) => p (Maybe String,String)
pWhiteBlack = (\t -> (mbTokWhite t,tokBlack t)) <$> pText

-------------------------------------------------------------------------
-- New Parser
-------------------------------------------------------------------------

type ShPr  c = (IsParser p Tok) => p c
type ShPr2 c = (IsParser p Tok) => p c -> p c
type ShPr3 c = (IsParser p Tok) => p c -> p c -> p c

mkNmForP :: String -> [String] -> Nm
mkNmForP h t = nmFromL . concat . map (wordsBy (=='.')) $ (h : t)

pAGItf :: ShPr T_AGItf
pAGItf = sem_AGItf_AGItf <$> (pFoldr (sem_Lines_Cons,sem_Lines_Nil) (sem_Line_AsIs sem_Words_Nil <$ pNl)) <*> pChunks

pVersion            ::  ShPr Version
pVersion            =   mkVerFromIntL <$> pList1Sep (pKey "_") pInt'

pVerOrder           ::  ShPr VersionOrder
pVerOrder           =   pListSep (pKey ",") (pList1Sep (pKey "<") pVersion)

pNm2                ::  ShPr Nm
pNm2                =   mkNmForP <$> p <*> pList (pKey "." *> (p <|> pInt))
                    where p = foldl1 (<|>) (map pKey kwTxtAsVarTooA) <|> pVar

pNm                 ::  ShPr Nm
pNm                 =   mkNmForP <$> pVar <*> pList (pKey "." *> (pVar <|> pInt))

pId, pIdNest, pIdNest2, pIdNestPart :: ShPr String
pId                 =   (\h t -> concat . intersperse "." $ h : t) <$> pVar <*> pList (pKey "." *> (pVar <|> pInt))
pIdNest             =   (concat . intersperse " ") <$> pList1 pIdNestPart
pIdNest2            =   (concat . intersperse " ") <$> pList1 (pIdNestPart <|> foldl1 (<|>) (map pKey kwTxtAsVarTooB))
pIdNestPart         =   pId
                    <|> (\l -> "(" ++ concat (intersperse "," l) ++ ")") <$> pParens pIdNests2
                    <|> pStr

pIdNests, pIdNests2 ::  ShPr [String]
pIdNests            =   pListSep (pKey ",") pIdNest
pIdNests2           =   pListSep (pKey ",") pIdNest2

pChunkId            ::  ShPr ChunkId
pChunkId            =   pVersion <+> (pKey "." *> pNm)

pParens             ::  ShPr2 p
pParens p           =   pKey "(" *> p <* pKey ")"

pChunks             ::  ShPr T_Chunks
pChunks             =   pFoldr (sem_Chunks_Cons,sem_Chunks_Nil) pChunk

pChunk              ::  ShPr T_Chunk 
pChunk              =   pBegChunk
                         *> ((   sem_Chunk_Ver
                                 <$> pVersion
                                 <*> pMaybe NmEmp id (pKey "." *> pNm)
                                 <*> (pKey "-" *> ((:[]) <$> pChunkId <|> pParens (pList1 pChunkId)) <|> pSucceed [])
                                 <*> pMbChKind
                                 <*> pMbChDest
                                 <*> (Just <$ pKey "module" <*> pId <|> pSucceed Nothing)
                                 <*> (pKey "import" *> pParens pIdNests2 <|> pSucceed [])
                                 <*> (pKey "export" *> pParens pIdNests2 <|> pSucceed [])
                             <|> sem_Chunk_Named
                                 <$> pNm
                             )
                             <*  pNl
                             <*> pLines
                             <*  pEndChunk
                             -- <*  pNl
                             <*> pLines
                            )
                    <?> "a chunk"

pChKind             ::  ShPr ChKind
pChKind             =   pAnyFromMap pKey chKindMp

pMbChKind           ::  ShPr ChKind
pMbChKind           =   pMaybe ChAG id pChKind

pChDest             ::  ShPr ChDest
pChDest             =   pAnyFromMap pKey chDestMp

pMbChDest           ::  ShPr ChDest
pMbChDest           =   pMaybe ChHere id pChDest

pChWrap             ::  ShPr ChWrap
pChWrap             =   pKey "wrap" *> pKey "=" *> pAnyFromMap pKey chWrapMp

pMbChWrap           ::  ShPr ChWrap
pMbChWrap           =   pMaybe ChWrapPlain id pChWrap

pLines              ::  ShPr T_Lines
pLines              =   pFoldr (sem_Lines_Cons,sem_Lines_Nil) pLine

pLine               ::  ShPr T_Line
pLine               =   sem_Line_AsIs  <$> pLineChars  <*  pNl
                    <|> (\n ((d,w),r)
                          -> sem_Line_Group 0 d w r (sem_Lines_Cons (sem_Line_Named n) sem_Lines_Nil))
                             <$  pBegNameRef <*> pN <*> pD <* pNl
                    <|> (\((d,w),r) l
                          -> sem_Line_Group 1 d w r l)
                             <$  pBegGroup   <*> pD <* pNl <*> pLines <* pEndChunk <* pNl
                    <?> "a line"
                    where pN =   pNm
                             <|> (\v n -> mkNm v `nmApd` n) <$> pVersion <*> pMaybe NmEmp id (pKey "." *> pNm)
                          pD =   pMbChDest
                             <+> pMbChWrap
                             <+> pMaybe Nothing Just ((pNm2 <|> mkNm <$> pStr) <+> pMaybe Nothing Just (pKey "=" *> pMaybe "" id pStr))

pLineChars          ::  ShPr T_Words
pLineChars          =   (foldr sem_Words_Cons sem_Words_Nil . concat)
                        <$> pList (   bwToWords1 sem_Word_Black
                                      <$> pWhiteBlack
                                  <|> bwToWords2 (\s -> sem_Word_Inline (sem_Inline_URI (tokBlack s)))
                                      <$> pBegInline <*> pText <* pEndInline
                                  )
                    where bwToWords1 sem (mw,b) =  maybe [] (\w -> [sem_Word_White w])                mw  ++ [sem b]
                          bwToWords2 sem  tk t  = (maybe [] (\w -> [sem_Word_White w]) $ mbTokWhite $ tk) ++ [sem t]

-------------------------------------------------------------------------
-- Parsing
-------------------------------------------------------------------------

parseHandle :: ShPr a -> FilePath -> Handle -> IO a
parseHandle p fn fh
  = do { txt <- hGetContents fh
       ; let toks = scan shuffleScanOpts ScSkip txt
       ; parseIOMessage show p toks
       }

parseAndGetRes :: ShPr a -> String -> a
parseAndGetRes p s
  = case evalSteps (parse p toks) of {Pair v _ -> v}
  where toks = scan shuffleScanOpts (ScLexMeta 0) s

